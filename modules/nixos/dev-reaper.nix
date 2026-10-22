{ pkgs, ... }:

let
  # Kills dev processes that nothing uses any more:
  #
  # 1. Dev servers (node, php, bun, esbuild) whose working directory is a
  #    worktree under ~/src/.worktrees that has been deleted. Removing a
  #    worktree doesn't stop its Vite or `php -S`; in Oct 2026 three Vite
  #    servers in deleted worktrees held 6 GB for three weeks. Shells, tmux and
  #    claude sessions are never touched, only the commands listed below.
  #
  # 2. atlas-idea's throwaway demo Postgres clusters (`atlas-demo-pg-*`, or
  #    ~/.cache/atlas/demo-pg) that the user's systemd has adopted, meaning
  #    the atlas-server that started them is gone, and that are over 12 hours
  #    old. `pg_ctl start` detaches them; 132 of these held 5 GB.
  #
  # Every kill is logged: journalctl --user -u dev-reaper
  # Dry run: dev-reaper --dry-run
  dev-reaper = pkgs.writeShellApplication {
    name = "dev-reaper";
    runtimeInputs = with pkgs; [ procps coreutils gnugrep gnused ];
    text = ''
      dry=0
      [ "''${1:-}" = "--dry-run" ] && dry=1
      worktrees="$HOME/src/.worktrees/"
      uid=$(id -u)
      killed=()
      dirs=()

      reap() { # signal pid reason
        echo "$3: pid $2 ($(ps -o args= -p "$2" | cut -c1-160))"
        [ "$dry" = 1 ] && return
        if kill "-$1" "$2" 2>/dev/null; then killed+=("$2"); fi
      }

      # Matched on the executable, not the process name: Node 24 names its
      # main thread "MainThread", and npm and npx run as node.
      for p in $(pgrep -u "$uid" .); do
        cwd=$(readlink "/proc/$p/cwd" 2>/dev/null) || continue
        case "$cwd" in "$worktrees"*" (deleted)") ;; *) continue ;; esac
        exe=$(readlink "/proc/$p/exe" 2>/dev/null) || continue
        case "$(basename "''${exe% (deleted)}")" in
          node|php|bun|esbuild) w=''${cwd#"$worktrees"}; reap TERM "$p" "deleted worktree ''${w% (deleted)}" ;;
        esac
      done

      for p in $(pgrep -u "$uid" -x postgres); do
        args=$(ps -o args= -p "$p") || continue
        echo "$args" | grep -qE ' -D [^ ]*(atlas-demo-pg-|atlas/demo-pg/)' || continue
        [ "$(ps -o comm= -p "$(ps -o ppid= -p "$p" | tr -d ' ')")" = systemd ] || continue
        [ "$(ps -o etimes= -p "$p" | tr -d ' ')" -gt 43200 ] || continue
        # SIGINT is Postgres's fast shutdown.
        reap INT "$p" "orphaned demo Postgres"
        data=$(echo "$args" | sed -E 's/.* -D ([^ ]+) .*/\1/')
        dir=$(dirname "$data")
        case "$dir" in
          /tmp/*/atlas-demo-pg-*|"$HOME"/.cache/atlas/demo-pg/pg-*) [ "$dry" = 1 ] || dirs+=("$dir") ;;
        esac
      done

      [ "$dry" = 1 ] && exit 0
      [ ''${#killed[@]} -eq 0 ] && { echo "nothing to reap"; exit 0; }
      for _ in $(seq 15); do
        alive=()
        for p in "''${killed[@]}"; do kill -0 "$p" 2>/dev/null && alive+=("$p"); done
        [ ''${#alive[@]} -eq 0 ] && break
        sleep 1
      done
      for p in "''${alive[@]}"; do echo "SIGKILL pid $p"; kill -KILL "$p" 2>/dev/null || true; done
      for d in "''${dirs[@]}"; do rm -rf "$d"; done
      echo "reaped ''${#killed[@]} processes"
    '';
  };
in
{
  environment.systemPackages = [ dev-reaper ];

  systemd.user.services.dev-reaper = {
    description = "Kill dev servers in deleted worktrees and orphaned demo Postgres";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${dev-reaper}/bin/dev-reaper";
    };
  };

  systemd.user.timers.dev-reaper = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "15min";
      OnUnitActiveSec = "30min";
    };
  };
}
