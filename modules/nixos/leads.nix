{ config, pkgs, lib, user, ... }:
{
  # ========================================
  # Lead pipeline (~/src/scraper)
  # ========================================
  # Finds music schools (Google Places), has Claude research each one, and
  # sends verified leads to n8n on garfield. Replaced the Fly app `overture`.
  #
  # It runs as a USER service, not a system one: the enricher drives an
  # interactive `claude` session, which needs this user's claude.ai login in
  # ~/.claude. Control it with the `leads` CLI (`leads status`, `leads run 10`);
  # see ~/src/scraper/README.md.

  # Postgres for the pipeline. Connects over the Unix socket with peer auth, so
  # there is no password: DATABASE_URL=postgresql://dustin@/leadscraper?host=/run/postgresql
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_17;
    ensureDatabases = [ "leadscraper" ];
    ensureUsers = [ { name = user; } ];
  };
  # ensureDBOwnership only works when the database is named after the user, so
  # hand ownership over explicitly. Idempotent; runs after the ensure* steps.
  systemd.services.postgresql-setup.script = lib.mkAfter ''
    psql -tAc 'ALTER DATABASE "leadscraper" OWNER TO "${user}";'
  '';

  # User services otherwise stop at logout and don't start until login.
  users.users.${user}.linger = true;

  # `leads` from any directory, pointed at the service's database.
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "leads" ''
      set -a
      . /home/${user}/.config/leadscraper/env
      set +a
      exec /home/${user}/src/scraper/.devenv/state/venv/bin/leads "$@"
    '')
  ];

  systemd.user.services.leads = {
    description = "Lead pipeline: scraper, AI enricher, n8n delivery";
    wantedBy = [ "default.target" ];

    # tmux and claude aren't on systemd's default PATH.
    path = [ pkgs.tmux pkgs.postgresql_17 "/home/${user}/.local" ];

    # No env file, no start (a skipped condition, not a failure, so no restart
    # loop). This is what kept the service from starting on an empty database
    # before the Fly data was restored.
    unitConfig.ConditionPathExists = "/home/${user}/.config/leadscraper/env";

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = "/home/${user}/src/scraper";
      # DATABASE_URL, GOOGLE_PLACES_API_KEY, N8N_WEBHOOK_URL (and _TOKEN if set).
      # Separate from the repo's .env, which is for local development.
      EnvironmentFile = "/home/${user}/.config/leadscraper/env";
      Environment = [
        # The enricher's claude sessions get their own tmux server, so they don't
        # land in whatever tmux session you happen to have open. Watch them with:
        #   tmux -S /run/user/1000/tmux-leads attach -t scraper-agents
        "ENRICHER_TMUX_SOCKET=/run/user/1000/tmux-leads"
      ];
      # A user unit can't order itself after the system postgresql.service, so
      # wait for the socket instead.
      ExecStartPre = [
        "${pkgs.bash}/bin/bash -c 'until ${pkgs.postgresql_17}/bin/pg_isready -q -h /run/postgresql; do sleep 1; done'"
        "/home/${user}/src/scraper/.devenv/state/venv/bin/alembic upgrade head"
      ];
      ExecStart = "/home/${user}/src/scraper/.devenv/state/venv/bin/leads serve";
      # Kills the tmux server and its claude sessions with the service. A school
      # that was in flight is released at the next start.
      ExecStopPost = "-${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-leads kill-server";
      Restart = "on-failure";
      RestartSec = 30;
      TimeoutStopSec = 30;
    };
  };
}
