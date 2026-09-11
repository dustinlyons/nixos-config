"""wow-chat-watch: tail World of Warcraft's chat log, match rules, notify.

The game client appends every chat line it displays to Logs/WoWChatLog.txt
while /chatlog is on (the AutoChatLog addon turns it on at login). This tool
follows that file, runs each message through the regex rules in rules.toml,
pops a desktop notification (notify-send) the first time a sender trips a
rule, and appends every match to a history file that `show` reads back.

Subcommands:
  watch   (default) follow the chat log, notify on matches, append to history
  replay  run the rules over the whole existing log and print matches; makes
          no notifications and writes nothing
  show    print past matches from the history file; --follow keeps printing
"""

from __future__ import annotations

import argparse
import html
import json
import logging
import os
import re
import subprocess
import sys
import time
import tomllib
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

LOG = logging.getLogger("wow-chat-watch")
DEFAULT_CONFIG = "~/.config/wow-chat-watch/rules.toml"
URGENCIES = ("low", "normal", "critical")

# ---------------------------------------------------------------------------
# Parsing WoWChatLog.txt
# ---------------------------------------------------------------------------
# Each line is the chat-frame text with colours stripped, after a timestamp:
#   9/11 20:15:32.456  [4. LookingForGroup] [Somedude]: LFM Gruul ...
#   9/11 20:15:40.101  [Guild] [Otherguy]: hi
#   9/11 20:15:41.000  [Somedude] says: hello
#   9/11 20:15:42.000  [Somedude-Realm] whispers: hey
#   9/11 20:15:43.000  To [Somedude]: hi
#   9/11 20:15:44.000  Somedude has come online.
# Player names are accepted with or without the brackets.

TIMESTAMP_RE = re.compile(
    r"^(\d{1,2}/\d{1,2})\s+(\d{1,2}:\d{2}:\d{2}(?:\.\d+)?)\s+(.*)$"
)
NUMBERED_RE = re.compile(r"^\[(\d+)\.\s*([^\]]+)\]\s+\[?([^\]:]+?)\]?:\s?(.*)$")
GROUP_RE = re.compile(
    r"^\[(Guild|Officer|Party|Party Leader|Raid|Raid Leader|Raid Warning"
    r"|Instance|Instance Leader)\]\s+\[?([^\]:]+?)\]?:\s?(.*)$",
    re.IGNORECASE,
)
SPEECH_RE = re.compile(
    r"^(?:\[([^\]]+?)\]|(\S+))\s+(says|yells|whispers):\s?(.*)$"
)
OUTGOING_RE = re.compile(r"^To\s+\[?([^\]:]+?)\]?:\s?(.*)$")
SPEECH_KIND = {"says": "say", "yells": "yell", "whispers": "whisper"}


@dataclass
class ChatLine:
    raw: str
    log_time: str  # "9/11 20:15:32.456" exactly as the game wrote it
    channel: str  # "LookingForGroup", "Guild", "say", "whisper", "system", ...
    sender: str
    text: str


def normalize_channel(name: str) -> str:
    """'Trade - City' -> 'Trade'; the suffix is the zone, not the channel."""
    return name.split(" - ", 1)[0].strip()


def parse_line(raw: str) -> ChatLine | None:
    """Split one log line into channel / sender / text; None if it has no timestamp."""
    m = TIMESTAMP_RE.match(raw)
    if not m:
        return None
    log_time = f"{m.group(1)} {m.group(2)}"
    body = m.group(3).strip()
    if n := NUMBERED_RE.match(body):
        return ChatLine(raw, log_time, normalize_channel(n.group(2)), n.group(3), n.group(4))
    if g := GROUP_RE.match(body):
        return ChatLine(raw, log_time, g.group(1), g.group(2), g.group(3))
    if o := OUTGOING_RE.match(body):
        return ChatLine(raw, log_time, "whisper", "You", o.group(2))
    if s := SPEECH_RE.match(body):
        sender = s.group(1) or s.group(2)
        return ChatLine(raw, log_time, SPEECH_KIND[s.group(3).lower()], sender, s.group(4))
    return ChatLine(raw, log_time, "system", "", body)


def log_time_to_epoch(log_time: str, year: int | None = None) -> float:
    """Best-effort epoch for a game timestamp (it has no year; assume this one)."""
    year = year or datetime.now().year
    for fmt in ("%Y/%m/%d %H:%M:%S.%f", "%Y/%m/%d %H:%M:%S"):
        try:
            return datetime.strptime(f"{year}/{log_time}", fmt).timestamp()
        except ValueError:
            continue
    return time.time()


# ---------------------------------------------------------------------------
# Config and rules
# ---------------------------------------------------------------------------


@dataclass
class Rule:
    name: str
    match: list[re.Pattern]
    require: list[re.Pattern]
    reject: list[re.Pattern]
    channels: set[str] | None  # lowercased; None = use the global list
    cooldown_s: float
    urgency: str

    def matches(self, text: str) -> bool:
        return (
            any(p.search(text) for p in self.match)
            and all(p.search(text) for p in self.require)
            and not any(p.search(text) for p in self.reject)
        )


@dataclass
class Config:
    log_file: Path
    channels: set[str]  # lowercased
    cooldown_s: float
    history_file: Path
    notify: bool
    sound: str
    rules: list[Rule]


class ConfigError(Exception):
    pass


def _patterns(rule: dict, key: str) -> list[re.Pattern]:
    raw = rule.get(key, [])
    if isinstance(raw, str):
        raw = [raw]
    out = []
    for p in raw:
        try:
            out.append(re.compile(p, re.IGNORECASE))
        except re.error as e:
            raise ConfigError(f"rule {rule.get('name')!r}: bad {key} pattern {p!r}: {e}")
    return out


def _channel_set(names) -> set[str]:
    return {normalize_channel(str(n)).lower() for n in names}


def load_config(path: Path) -> Config:
    try:
        data = tomllib.loads(path.read_text())
    except FileNotFoundError:
        raise ConfigError(f"config not found: {path}")
    except tomllib.TOMLDecodeError as e:
        raise ConfigError(f"{path}: {e}")

    watch = data.get("watch", {})
    alert = data.get("alert", {})
    cooldown_s = float(watch.get("cooldown_minutes", 30)) * 60

    rules = []
    for r in data.get("rules", []):
        name = r.get("name")
        if not name or not r.get("match"):
            raise ConfigError(f"every rule needs a name and a match list: {r}")
        urgency = str(r.get("urgency", "normal"))
        if urgency not in URGENCIES:
            raise ConfigError(f"rule {name!r}: urgency must be one of {URGENCIES}")
        rules.append(
            Rule(
                name=str(name),
                match=_patterns(r, "match"),
                require=_patterns(r, "require"),
                reject=_patterns(r, "reject"),
                channels=_channel_set(r["channels"]) if "channels" in r else None,
                cooldown_s=float(r.get("cooldown_minutes", cooldown_s / 60)) * 60,
                urgency=urgency,
            )
        )
    if not rules:
        raise ConfigError(f"{path}: no [[rules]] defined")

    return Config(
        log_file=Path(os.path.expanduser(watch.get("log_file", ""))),
        channels=_channel_set(watch.get("channels", ["LookingForGroup", "World", "Trade", "General"])),
        cooldown_s=cooldown_s,
        history_file=Path(os.path.expanduser(watch.get("history_file", "~/.local/state/wow-chat-watch/matches.jsonl"))),
        notify=bool(alert.get("notify", True)),
        sound=str(alert.get("sound", "")),
        rules=rules,
    )


# ---------------------------------------------------------------------------
# Matching, cooldowns, history, notifications
# ---------------------------------------------------------------------------


@dataclass
class Match:
    rule: Rule
    line: ChatLine
    notified: bool  # False = same sender tripped this rule inside the cooldown
    ts: float

    def record(self) -> dict:
        return {
            "ts": self.ts,
            "log_time": self.line.log_time,
            "rule": self.rule.name,
            "channel": self.line.channel,
            "sender": self.line.sender,
            "text": self.line.text,
            "notified": self.notified,
        }


class Watcher:
    def __init__(self, cfg: Config):
        self.cfg = cfg
        self.last_alert: dict[tuple[str, str], float] = {}

    def seed_from_history(self, limit: int = 2000) -> None:
        """Restore cooldowns from the history file so a restart doesn't re-notify."""
        for rec in read_history(self.cfg.history_file)[-limit:]:
            if rec.get("notified"):
                key = (rec.get("rule", ""), str(rec.get("sender", "")).lower())
                self.last_alert[key] = max(self.last_alert.get(key, 0.0), float(rec.get("ts", 0)))

    def process(self, line: ChatLine, now: float) -> list[Match]:
        found = []
        for rule in self.cfg.rules:
            allowed = rule.channels if rule.channels is not None else self.cfg.channels
            if line.channel.lower() not in allowed:
                continue
            if not rule.matches(line.text):
                continue
            key = (rule.name, line.sender.lower())
            last = self.last_alert.get(key)
            notified = last is None or now - last >= rule.cooldown_s
            if notified:
                self.last_alert[key] = now
            found.append(Match(rule, line, notified, now))
        return found


def append_history(path: Path, m: Match) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(m.record(), ensure_ascii=False) + "\n")


def read_history(path: Path) -> list[dict]:
    try:
        lines = path.read_text(encoding="utf-8").splitlines()
    except FileNotFoundError:
        return []
    out = []
    for ln in lines:
        try:
            out.append(json.loads(ln))
        except json.JSONDecodeError:
            continue
    return out


def send_notification(m: Match, cfg: Config) -> None:
    title = m.rule.name
    # notify-send treats the body as Pango markup, so a "<Guild Name>" in chat
    # would vanish as an unknown tag unless escaped.
    body = html.escape(f"[{m.line.channel}] {m.line.sender}: {m.line.text}")
    cmd = ["notify-send", "--app-name=WoW Chat", f"--urgency={m.rule.urgency}", "--icon=dialog-information"]
    if m.rule.urgency != "critical":
        cmd.append("--expire-time=30000")
    cmd += ["--", title, body]
    try:
        subprocess.run(cmd, check=False, timeout=10)
    except (OSError, subprocess.TimeoutExpired) as e:
        LOG.warning("notify-send failed: %s", e)
    if cfg.sound:
        try:
            subprocess.Popen(["pw-play", cfg.sound], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except OSError as e:
            LOG.warning("pw-play failed: %s", e)


# ---------------------------------------------------------------------------
# Following files
# ---------------------------------------------------------------------------


def follow(path: Path, poll: float = 0.5, from_start: bool = False):
    """Yield lines appended to `path`, like `tail -F`.

    Waits for the file to appear, and reopens from the top if it is truncated,
    deleted, or replaced. With from_start=False, content that exists when
    follow() is called is skipped (the open happens eagerly, not on first next()).
    """
    fh = None
    try:
        fh = open(path, "rb")
        if not from_start:
            fh.seek(0, os.SEEK_END)
    except FileNotFoundError:
        pass
    return _follow_lines(path, poll, fh)


def _follow_lines(path: Path, poll: float, fh):
    inode = os.fstat(fh.fileno()).st_ino if fh else None
    buf = b""
    try:
        while True:
            if fh is None:
                try:
                    fh = open(path, "rb")  # a file that appears later is new: read it all
                except FileNotFoundError:
                    time.sleep(poll * 4)
                    continue
                inode = os.fstat(fh.fileno()).st_ino
                buf = b""
            chunk = fh.read()
            if chunk:
                buf += chunk
                lines = buf.split(b"\n")
                buf = lines.pop()  # keep the partial last line for next time
                for raw in lines:
                    yield raw.decode("utf-8", "replace").rstrip("\r")
                continue
            try:
                st = os.stat(path)
                stale = st.st_ino != inode or st.st_size < fh.tell()
            except FileNotFoundError:
                stale = True
            if stale:
                fh.close()
                fh = None
                continue
            time.sleep(poll)
    finally:
        if fh is not None:
            fh.close()


def read_lines(path: Path):
    with open(path, "rb") as fh:
        for raw in fh:
            yield raw.decode("utf-8", "replace").rstrip("\r\n")


# ---------------------------------------------------------------------------
# Commands
# ---------------------------------------------------------------------------

PALETTE = ("\033[36m", "\033[33m", "\033[35m", "\033[32m", "\033[34m", "\033[31m")
BOLD, DIM, RESET = "\033[1m", "\033[2m", "\033[0m"


def fmt_record(rec: dict, color: bool) -> str:
    try:
        when = datetime.fromtimestamp(float(rec["ts"])).strftime("%m-%d %H:%M")
    except (KeyError, ValueError, OSError):
        when = str(rec.get("log_time", "?"))[:11]
    rule = str(rec.get("rule", ""))
    mark = "!" if rec.get("notified") else " "
    head = f"{when}  {mark} {rule:<22.22} {rec.get('channel', ''):<16.16} {rec.get('sender', ''):<18.18} "
    text = str(rec.get("text", ""))
    if not color:
        return head + text
    c = PALETTE[sum(map(ord, rule)) % len(PALETTE)]
    line = f"{DIM}{when}{RESET}  {mark} {c}{BOLD}{rule:<22.22}{RESET} {DIM}{rec.get('channel', ''):<16.16}{RESET} {rec.get('sender', ''):<18.18} {text}"
    return f"{DIM}{line}{RESET}" if not rec.get("notified") else line


def cmd_watch(args, cfg: Config) -> int:
    notify = cfg.notify and not args.no_notify
    LOG.info("following %s", cfg.log_file)
    LOG.info("%d rule(s); channels %s; notifications %s; history %s",
             len(cfg.rules), sorted(cfg.channels), "on" if notify else "off", cfg.history_file)
    if not cfg.log_file.exists():
        LOG.info("log file does not exist yet; waiting for the game to create it")
    watcher = Watcher(cfg)
    watcher.seed_from_history()
    for raw in follow(cfg.log_file, from_start=args.from_start):
        line = parse_line(raw)
        if line is None:
            continue
        for m in watcher.process(line, time.time()):
            append_history(cfg.history_file, m)
            LOG.info("%s %s [%s] %s: %s", "NOTIFY" if m.notified else "quiet ", m.rule.name, line.channel, line.sender, line.text)
            if m.notified and notify:
                send_notification(m, cfg)
    return 0


def cmd_replay(args, cfg: Config) -> int:
    if not cfg.log_file.exists():
        print(f"no log file at {cfg.log_file}", file=sys.stderr)
        return 1
    color = sys.stdout.isatty()
    watcher = Watcher(cfg)
    total = parsed = hits = notified = 0
    for raw in read_lines(cfg.log_file):
        total += 1
        line = parse_line(raw)
        if line is None:
            if args.verbose:
                print(f"{DIM if color else ''}?? {raw}{RESET if color else ''}")
            continue
        parsed += 1
        if args.verbose:
            print(f"   {line.log_time}  [{line.channel}] {line.sender}: {line.text}")
        for m in watcher.process(line, log_time_to_epoch(line.log_time)):
            hits += 1
            notified += m.notified
            if m.notified or args.all:
                print(fmt_record(m.record(), color))
    sys.stdout.flush()  # keep the summary after the matches when stdout is a pipe
    print(f"-- {total} lines, {parsed} parsed as chat, {hits} matches, {notified} would notify", file=sys.stderr)
    return 0


def cmd_show(args, cfg: Config) -> int:
    color = sys.stdout.isatty()
    path = cfg.history_file
    records = read_history(path)
    if not args.all:
        records = [r for r in records if r.get("notified")]
    if not records and not args.follow:
        print(f"no matches recorded yet ({path})", file=sys.stderr)
        return 0
    for rec in records[-args.limit:]:
        print(fmt_record(rec, color))
    if not args.follow:
        return 0
    sys.stdout.flush()
    for raw in follow(path):
        try:
            rec = json.loads(raw)
        except json.JSONDecodeError:
            continue
        if args.all or rec.get("notified"):
            print(fmt_record(rec, color), flush=True)
    return 0


def build_parser() -> argparse.ArgumentParser:
    ap = argparse.ArgumentParser(prog="wow-chat-watch", description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--config", default=DEFAULT_CONFIG, help=f"rules file (default {DEFAULT_CONFIG})")
    ap.add_argument("-v", "--verbose", action="store_true")
    sub = ap.add_subparsers(dest="cmd")

    w = sub.add_parser("watch", help="follow the chat log and notify (default)")
    w.add_argument("--no-notify", action="store_true", help="record matches but skip notify-send")
    w.add_argument("--from-start", action="store_true", help="process the existing log content before following")

    r = sub.add_parser("replay", help="run the rules over the whole existing log; no side effects")
    r.add_argument("--all", action="store_true", help="also print matches the cooldown would have silenced")

    s = sub.add_parser("show", help="print past matches")
    s.add_argument("-n", "--limit", type=int, default=40, help="how many recent matches to print (default 40)")
    s.add_argument("-f", "--follow", action="store_true", help="keep printing new matches as they happen")
    s.add_argument("--all", action="store_true", help="include matches that were silenced by a cooldown")
    return ap


def main(argv: list[str] | None = None) -> int:
    args = build_parser().parse_args(argv)
    logging.basicConfig(level=logging.DEBUG if args.verbose else logging.INFO, format="%(levelname)s %(message)s", stream=sys.stderr)
    cmd = args.cmd or "watch"
    for name, default in (("no_notify", False), ("from_start", False), ("all", False), ("limit", 40), ("follow", False)):
        if not hasattr(args, name):
            setattr(args, name, default)
    try:
        cfg = load_config(Path(os.path.expanduser(args.config)))
    except ConfigError as e:
        print(f"wow-chat-watch: {e}", file=sys.stderr)
        return 2
    try:
        return {"watch": cmd_watch, "replay": cmd_replay, "show": cmd_show}[cmd](args, cfg)
    except KeyboardInterrupt:
        return 0
    except BrokenPipeError:
        return 0


if __name__ == "__main__":
    sys.exit(main())
