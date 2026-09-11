"""Unit tests for wow_chat_watch. Run from this directory: python3 -m unittest -v"""

import json
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock

import wow_chat_watch as w

RULES = """
[watch]
log_file = "{log}"
channels = ["LookingForGroup", "World"]
cooldown_minutes = 30
history_file = "{history}"

[alert]
notify = true
sound = ""

[[rules]]
name = "Gruul"
match = ['\\bgruul']
reject = ['\\bgdkp\\b']
urgency = "critical"

[[rules]]
name = "Guild recruiting DPS"
match = ['\\brecruit', '\\blooking\\s+for\\b']
require = ['\\bdps\\b']
channels = ["World", "Trade"]
cooldown_minutes = 1
"""


class ParseTests(unittest.TestCase):
    def check(self, raw, channel, sender, text):
        line = w.parse_line(raw)
        self.assertIsNotNone(line, raw)
        self.assertEqual((line.channel, line.sender, line.text), (channel, sender, text))
        return line

    def test_numbered_channel_bracketed_name(self):
        line = self.check(
            "9/11 20:15:32.456  [4. LookingForGroup] [Somedude]: LFM Gruul need 2 heal",
            "LookingForGroup", "Somedude", "LFM Gruul need 2 heal",
        )
        self.assertEqual(line.log_time, "9/11 20:15:32.456")

    def test_numbered_channel_plain_name_and_zone_suffix(self):
        self.check(
            "9/11 20:15:32.456  [2. Trade - City] Somedude-Realm: WTS [Item]",
            "Trade", "Somedude-Realm", "WTS [Item]",
        )

    def test_guild(self):
        self.check("9/11 20:15:40.101  [Guild] [Otherguy]: hi", "Guild", "Otherguy", "hi")

    def test_say_yell_whisper(self):
        self.check("9/11 20:15:41.000  [Somedude] says: hello", "say", "Somedude", "hello")
        self.check("9/11 20:15:41.000  Somedude yells: hello", "yell", "Somedude", "hello")
        self.check("9/11 20:15:42.000  [Somedude-Realm] whispers: hey", "whisper", "Somedude-Realm", "hey")
        self.check("9/11 20:15:42.000  [Innkeeper Allison] says: Welcome", "say", "Innkeeper Allison", "Welcome")

    def test_outgoing_whisper(self):
        self.check("9/11 20:15:43.000  To [Somedude]: hi", "whisper", "You", "hi")

    def test_system_message(self):
        self.check("9/11 20:15:44.000  Somedude has come online.", "system", "", "Somedude has come online.")

    def test_no_timestamp(self):
        self.assertIsNone(w.parse_line("garbage line"))
        self.assertIsNone(w.parse_line(""))

    def test_timestamp_without_millis(self):
        self.check("9/11 20:15:44  [1. General] [A]: b", "General", "A", "b")

    def test_log_time_to_epoch_roundtrip(self):
        ts = w.log_time_to_epoch("9/11 20:15:32.456", year=2026)
        self.assertEqual(w.datetime.fromtimestamp(ts).strftime("%m/%d %H:%M:%S"), "09/11 20:15:32")


class ConfigCase(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.dir = Path(self.tmp.name)
        self.log = self.dir / "WoWChatLog.txt"
        self.history = self.dir / "matches.jsonl"
        self.rules = self.dir / "rules.toml"
        self.rules.write_text(RULES.format(log=self.log, history=self.history))
        self.cfg = w.load_config(self.rules)

    def tearDown(self):
        self.tmp.cleanup()


class ConfigTests(ConfigCase):
    def test_loads(self):
        self.assertEqual(self.cfg.channels, {"lookingforgroup", "world"})
        self.assertEqual(self.cfg.cooldown_s, 1800)
        self.assertEqual([r.name for r in self.cfg.rules], ["Gruul", "Guild recruiting DPS"])
        self.assertEqual(self.cfg.rules[0].urgency, "critical")
        self.assertEqual(self.cfg.rules[0].cooldown_s, 1800)
        self.assertEqual(self.cfg.rules[1].cooldown_s, 60)
        self.assertEqual(self.cfg.rules[1].channels, {"world", "trade"})
        self.assertIsNone(self.cfg.rules[0].channels)

    def test_rule_matching(self):
        gruul, guild = self.cfg.rules
        self.assertTrue(gruul.matches("LFM GRUUL 2 heals"))
        self.assertTrue(gruul.matches("lfm gruul's lair"))
        self.assertFalse(gruul.matches("LFM Gruul GDKP"))
        self.assertFalse(gruul.matches("LFM Kara"))
        self.assertTrue(guild.matches("<Guild> is recruiting DPS for T5"))
        self.assertTrue(guild.matches("<Guild> looking for dps and heals"))
        self.assertFalse(guild.matches("<Guild> recruiting healers"))
        self.assertFalse(guild.matches("dps lf guild"))

    def test_errors(self):
        for bad in (
            RULES + "\n[[rules]]\nmatch = ['x']\n",  # no name
            RULES + "\n[[rules]]\nname = 'x'\nmatch = ['(']\n",  # bad regex
            RULES + "\n[[rules]]\nname = 'x'\nmatch = ['x']\nurgency = 'loud'\n",
            "not = [toml",
        ):
            self.rules.write_text(bad.format(log=self.log, history=self.history) if "{log}" in bad else bad)
            with self.assertRaises(w.ConfigError):
                w.load_config(self.rules)
        with self.assertRaises(w.ConfigError):
            w.load_config(self.dir / "missing.toml")


class WatcherTests(ConfigCase):
    def line(self, channel, sender, text):
        return w.ChatLine("", "9/11 20:00:00.000", channel, sender, text)

    def test_channel_filter_global_and_per_rule(self):
        watcher = w.Watcher(self.cfg)
        self.assertEqual(watcher.process(self.line("Guild", "A", "gruul"), 0), [])
        self.assertEqual(len(watcher.process(self.line("LookingForGroup", "A", "gruul"), 0)), 1)
        # rule 2 overrides channels to World/Trade: Trade is fine even though
        # the global list excludes it, LookingForGroup is not.
        self.assertEqual(len(watcher.process(self.line("Trade", "B", "recruiting dps"), 0)), 1)
        self.assertEqual(watcher.process(self.line("LookingForGroup", "B", "recruiting dps"), 0), [])

    def test_cooldown_per_sender_per_rule(self):
        watcher = w.Watcher(self.cfg)
        first = watcher.process(self.line("LookingForGroup", "Somedude", "LFM gruul"), 1000)
        self.assertTrue(first[0].notified)
        again = watcher.process(self.line("LookingForGroup", "somedude", "LFM gruul 1 more"), 1000 + 60)
        self.assertFalse(again[0].notified)  # same sender (case-insensitive), inside 30 min
        other = watcher.process(self.line("LookingForGroup", "Else", "LFM gruul"), 1000 + 60)
        self.assertTrue(other[0].notified)
        later = watcher.process(self.line("LookingForGroup", "Somedude", "LFM gruul"), 1000 + 1800)
        self.assertTrue(later[0].notified)

    def test_per_rule_cooldown_override(self):
        watcher = w.Watcher(self.cfg)
        self.assertTrue(watcher.process(self.line("World", "G", "recruiting dps"), 0)[0].notified)
        self.assertFalse(watcher.process(self.line("World", "G", "recruiting dps"), 30)[0].notified)
        self.assertTrue(watcher.process(self.line("World", "G", "recruiting dps"), 61)[0].notified)

    def test_one_message_two_rules(self):
        watcher = w.Watcher(self.cfg)
        found = watcher.process(self.line("World", "G", "<Guild> recruiting dps for gruul"), 0)
        self.assertEqual([m.rule.name for m in found], ["Gruul", "Guild recruiting DPS"])

    def test_history_roundtrip_and_seed(self):
        watcher = w.Watcher(self.cfg)
        for m in watcher.process(self.line("LookingForGroup", "Somedude", "LFM gruul"), 5000):
            w.append_history(self.history, m)
        recs = w.read_history(self.history)
        self.assertEqual(len(recs), 1)
        self.assertEqual(recs[0]["sender"], "Somedude")
        self.assertTrue(recs[0]["notified"])
        # A fresh watcher (as after a service restart) inherits the cooldown.
        fresh = w.Watcher(self.cfg)
        fresh.seed_from_history()
        self.assertFalse(fresh.process(self.line("LookingForGroup", "Somedude", "LFM gruul"), 5000 + 10)[0].notified)

    def test_read_history_skips_bad_lines(self):
        self.history.write_text('{"rule": "a", "notified": true}\nnot json\n')
        self.assertEqual(len(w.read_history(self.history)), 1)
        self.assertEqual(w.read_history(self.dir / "nope.jsonl"), [])


class NotificationTests(ConfigCase):
    def test_notify_send_command_and_escaping(self):
        m = w.Match(self.cfg.rules[1], w.ChatLine("", "t", "World", "Guy", "<Big Guild> recruiting dps & heals"), True, 0)
        with mock.patch.object(subprocess, "run") as run, mock.patch.object(subprocess, "Popen") as popen:
            w.send_notification(m, self.cfg)
        cmd = run.call_args.args[0]
        self.assertEqual(cmd[0], "notify-send")
        self.assertIn("--urgency=normal", cmd)
        self.assertIn("--expire-time=30000", cmd)
        self.assertEqual(cmd[-2], "Guild recruiting DPS")
        self.assertEqual(cmd[-1], "[World] Guy: &lt;Big Guild&gt; recruiting dps &amp; heals")
        popen.assert_not_called()  # sound = ""

    def test_critical_has_no_expiry_and_sound_plays(self):
        self.cfg.sound = "/some/sound.oga"
        m = w.Match(self.cfg.rules[0], w.ChatLine("", "t", "LookingForGroup", "A", "gruul"), True, 0)
        with mock.patch.object(subprocess, "run") as run, mock.patch.object(subprocess, "Popen") as popen:
            w.send_notification(m, self.cfg)
        cmd = run.call_args.args[0]
        self.assertIn("--urgency=critical", cmd)
        self.assertNotIn("--expire-time=30000", cmd)
        self.assertEqual(popen.call_args.args[0], ["pw-play", "/some/sound.oga"])

    def test_notify_send_missing_is_logged_not_fatal(self):
        m = w.Match(self.cfg.rules[0], w.ChatLine("", "t", "LookingForGroup", "A", "gruul"), True, 0)
        with mock.patch.object(subprocess, "run", side_effect=FileNotFoundError("notify-send")):
            with self.assertLogs(w.LOG, level="WARNING"):
                w.send_notification(m, self.cfg)


class FollowTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.path = Path(self.tmp.name) / "log.txt"

    def tearDown(self):
        if getattr(self, "gen", None) is not None:
            self.gen.close()
        self.tmp.cleanup()

    def follow(self, **kw):
        self.gen = w.follow(self.path, poll=0.01, **kw)
        return self.gen

    def append(self, text):
        with open(self.path, "ab") as fh:
            fh.write(text.encode())

    def test_skips_existing_then_yields_new_and_handles_partial_lines(self):
        self.append("old1\r\nold2\r\n")
        gen = self.follow()
        self.append("new1\r\nnew2\r\npart")
        self.assertEqual(next(gen), "new1")
        self.assertEqual(next(gen), "new2")
        self.append("ial\r\n")
        self.assertEqual(next(gen), "partial")

    def test_from_start_reads_existing(self):
        self.append("a\nb\n")
        gen = self.follow(from_start=True)
        self.assertEqual([next(gen), next(gen)], ["a", "b"])

    def test_waits_for_missing_file_then_reads_it_from_the_top(self):
        gen = self.follow()  # file does not exist yet
        self.append("first\n")
        self.assertEqual(next(gen), "first")

    def test_truncation_reopens_from_top(self):
        self.append("x\n")
        gen = self.follow(from_start=True)
        self.assertEqual(next(gen), "x")
        self.append("longer line\n")
        self.assertEqual(next(gen), "longer line")
        self.path.write_text("t\n")  # truncate + shorter content
        self.assertEqual(next(gen), "t")

    def test_replacement_reopens_new_file(self):
        self.append("x\n")
        gen = self.follow(from_start=True)
        self.assertEqual(next(gen), "x")
        os.unlink(self.path)
        self.append("fresh\n")
        self.assertEqual(next(gen), "fresh")


class CliTests(ConfigCase):
    def test_replay_prints_matches_and_summary(self):
        self.log.write_text(
            "9/11 20:15:32.456  [4. LookingForGroup] [Somedude]: LFM Gruul need 2 heal 3 dps, HR DST\r\n"
            "9/11 20:15:40.101  [4. LookingForGroup] [Somedude]: LFM Gruul need 1 heal\r\n"
            "9/11 20:16:00.000  [1. World] [Guildy]: <Guild> recruiting DPS for T5, pst\r\n"
            "9/11 20:16:05.000  [2. Trade - City] [Vendor]: WTS [Thing]\r\n"
            "9/11 20:16:06.000  Somedude has come online.\r\n"
            "not a log line\r\n"
        )
        with mock.patch("sys.stdout") as out, mock.patch("sys.stderr") as err:
            out.isatty.return_value = False
            rc = w.main(["--config", str(self.rules), "replay"])
        self.assertEqual(rc, 0)
        printed = "".join(c.args[0] for c in out.write.call_args_list)
        self.assertIn("! Gruul", printed)
        self.assertIn("Guild recruiting DPS", printed)
        self.assertNotIn("need 1 heal", printed)  # silenced by the cooldown
        summary = "".join(c.args[0] for c in err.write.call_args_list)
        self.assertIn("6 lines, 5 parsed as chat, 3 matches, 2 would notify", summary)
        self.assertFalse(self.history.exists())  # replay never writes

    def test_show_reads_history(self):
        recs = [
            {"ts": 1_700_000_000, "rule": "Gruul", "channel": "LookingForGroup", "sender": "A", "text": "one", "notified": True},
            {"ts": 1_700_000_100, "rule": "Gruul", "channel": "LookingForGroup", "sender": "A", "text": "two", "notified": False},
        ]
        self.history.write_text("".join(json.dumps(r) + "\n" for r in recs))
        with mock.patch("sys.stdout") as out:
            out.isatty.return_value = False
            w.main(["--config", str(self.rules), "show"])
            default = "".join(c.args[0] for c in out.write.call_args_list)
            out.write.reset_mock()
            w.main(["--config", str(self.rules), "show", "--all"])
            everything = "".join(c.args[0] for c in out.write.call_args_list)
        self.assertIn("one", default)
        self.assertNotIn("two", default)
        self.assertIn("two", everything)

    def test_bad_config_exit_code(self):
        with mock.patch("sys.stderr"):
            self.assertEqual(w.main(["--config", str(self.dir / "nope.toml"), "show"]), 2)


if __name__ == "__main__":
    unittest.main()
