{ config, pkgs, lib, ... }:

let
  user = "dustin";

  # The rev-ops checkout is the off-box target: it already holds the workflow
  # JSON exports and RECOVERY.md, and garfield already authenticates to GitHub
  # as ${user}. Nothing new has to be trusted for this to reach a second site.
  repo = "/home/${user}/.local/share/src/rev-ops";

  # age recipients for everything that can't be committed in the clear.
  #
  # The first key is the one that matters: it is present on BOTH felix and
  # garfield today (verified — same key, same comment, both machines), so an
  # encrypted backup stays readable after this host is gone. That is the whole
  # point, and it is why the list does not stop at the host key.
  #
  # The second is the off-box anchor named in nix-secrets/secrets.nix. It is
  # included so these backups become readable the moment that key resurfaces;
  # it is deliberately NOT the only personal recipient, because as of
  # 2026-08-31 that private key could not be found on either machine.
  #
  # The third lets garfield itself decrypt without a human, for a same-host
  # restore (service corruption rather than hardware loss).
  ageRecipients = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAMa1s9TGyJktClvNLvvC1OHQwwXmS81Pr/qFyTiMzbi dustin@dlyons.dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIO/RZ2j8AcRxzlkW0C4A/nABQbR/7ie5nrBXm/aZ6PpS dustin@dlyons.dev"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJsWsJX0vAXLYYGhNT6adRRz4iHJahpvMPE+5BLse3Bz root@nixos"
  ];
  ageArgs = lib.concatMapStringsSep " " (k: "-r ${lib.escapeShellArg k}") ageRecipients;

  backupScript = pkgs.writeShellApplication {
    name = "gtm-backup";
    runtimeInputs = with pkgs; [
      age
      coreutils
      git
      gnutar
      gzip
      gnused
      jq
      util-linux
      config.services.postgresql.package
    ];
    text = ''
      REPO=${lib.escapeShellArg repo}
      WORKFLOWS="$REPO/workflows"
      BACKUPS="$REPO/backups"

      if [ ! -d "$REPO/.git" ]; then
        echo "No git checkout at $REPO — nothing to back up into." >&2
        exit 1
      fi

      mkdir -p "$WORKFLOWS" "$BACKUPS"
      WORK=$(mktemp -d)
      trap 'rm -rf "$WORK"' EXIT

      # age output is randomised per run, so re-encrypting unchanged input
      # would produce a new ciphertext — and a pointless commit — every night.
      # Encrypt only when the plaintext's hash actually moved.
      encrypt_if_changed() {
        local plain="$1" out="$2" sum
        sum=$(sha256sum < "$plain" | cut -d' ' -f1)
        if [ -f "$out" ] && [ -f "$out.sha256" ] && [ "$(cat "$out.sha256")" = "$sum" ]; then
          echo "  = $(basename "$out") unchanged"
          return
        fi
        age ${ageArgs} -o "$out" < "$plain"
        printf '%s\n' "$sum" > "$out.sha256"
        echo "  + $(basename "$out")"
      }

      # ---- 1. Workflow JSON exports -------------------------------------
      # Read straight from PostgreSQL rather than the n8n REST API, so this
      # needs no API token and cannot break when a token is rotated. The
      # selected fields and the slug rules match rev-ops/backup.sh exactly, so
      # filenames stay stable and restore.sh keeps working unchanged.
      echo "Exporting workflows from PostgreSQL..."
      runuser -u postgres -- psql -d n8n -At -F'|' \
        -c 'select id, name from workflow_entity where "isArchived" = false order by name' \
        > "$WORK/list"

      # Read the list on fd 3, not stdin: the loop body shells out to psql and
      # git, and a child inheriting stdin would eat the remaining rows.
      count=0
      while IFS='|' read -r -u 3 id name; do
        [ -n "$id" ] || continue
        slug=$(printf '%s' "$name" \
          | tr '[:upper:]' '[:lower:]' \
          | sed 's/[^a-z0-9]/-/g; s/--*/-/g; s/^-//; s/-$//')
        runuser -u postgres -- psql -d n8n -At \
          -c "select json_build_object(
                'id', id, 'name', name, 'active', active,
                'nodes', nodes, 'connections', connections,
                'settings', settings, 'staticData', \"staticData\",
                'pinData', \"pinData\", 'meta', meta)
              from workflow_entity where id = '$id'" \
          | jq -S . > "$WORKFLOWS/$slug.json"
        echo "  ✓ $name → $slug.json"
        count=$((count + 1))
      done 3< "$WORK/list"
      echo "Exported $count workflows."

      # ---- 2. n8n database dump -----------------------------------------
      # Workflow JSON does not carry credential records; this does, which is
      # what makes the agenix-held encryption key worth having. execution_data
      # is 98% of the database (1.07 GB of the 1.09 GB) and is pure history —
      # schema is kept, rows are dropped.
      echo "Dumping n8n database..."
      runuser -u postgres -- pg_dump -d n8n \
        --exclude-table-data='execution_data' \
        --exclude-table-data='execution_entity' \
        --exclude-table-data='insights_by_period' \
        > "$WORK/n8n.sql"
      gzip -9nc "$WORK/n8n.sql" > "$WORK/n8n.sql.gz"
      encrypt_if_changed "$WORK/n8n.sql.gz" "$BACKUPS/n8n-db.sql.gz.age"

      # ---- 3. Home Assistant state --------------------------------------
      # .storage is the irreplaceable part: the login, the UniFi Protect
      # config entry (which is where the Protect credentials actually live —
      # secrets.yaml was never created), and the entity registry that makes
      # camera.garage resolve to a real camera.
      #
      # Excluded on purpose: deps/ and tts/ are rebuildable caches, and the
      # recorder database is disposable sensor history that would otherwise be
      # copied mid-write from a running SQLite file.
      if [ -d /var/lib/hass ]; then
        echo "Archiving Home Assistant state..."
        tar -czf "$WORK/hass.tar.gz" -C /var/lib/hass \
          --sort=name --owner=0 --group=0 --numeric-owner --mtime='@0' \
          --exclude=deps --exclude=tts --exclude=garage-snapshots \
          --exclude='*.log' --exclude='*.log.*' --exclude='*.fault' \
          --exclude='home-assistant_v2.db*' --exclude='.ha_run.lock' \
          .
        encrypt_if_changed "$WORK/hass.tar.gz" "$BACKUPS/home-assistant.tar.gz.age"
      fi

      # ---- 4. Credential inventory ---------------------------------------
      # secrets.md is gitignored, which is correct — but it is also the one
      # file RECOVERY.md lists as a prerequisite, so in plaintext-only form it
      # was the gap in an otherwise complete plan. Encrypted, it can travel.
      if [ -f "$REPO/secrets.md" ]; then
        encrypt_if_changed "$REPO/secrets.md" "$BACKUPS/secrets.md.age"
      fi

      # ---- 5. Commit and push --------------------------------------------
      chown -R ${user}:users "$WORKFLOWS" "$BACKUPS"

      # Explicit paths, never `add -A`: the checkout carries untracked network
      # diagrams and a gitignored secrets.md that must not be swept in.
      runuser -u ${user} -- git -C "$REPO" add workflows backups

      if runuser -u ${user} -- git -C "$REPO" diff --cached --quiet; then
        echo "No changes to commit."
        exit 0
      fi

      runuser -u ${user} -- git -C "$REPO" \
        commit -q -m "backup: automated export from garfield"
      echo "Committed."

      if runuser -u ${user} -- git -C "$REPO" push -q origin HEAD; then
        echo "Pushed to origin."
      else
        echo "PUSH FAILED — the commit is local only, off-box copy is stale." >&2
        exit 1
      fi
    '';
  };
in
{
  # ========================================
  # GTM / Home Assistant backups
  # ========================================
  # Everything on this host that `nixos-rebuild` cannot recreate, pushed to a
  # second site nightly.
  #
  # Why it exists: the n8n workflows were exported by hand via
  # rev-ops/backup.sh, so the off-box copy was only as fresh as the last time
  # someone remembered to run it (last run 2026-06-26, found 2026-08-31).
  # Home Assistant's state was not covered at all.
  #
  # Restore path: RECOVERY.md in the rev-ops repo.
  #
  # NOT covered here, because it is not this host's state: the UDM's port
  # forwards, its static DNS record for jenkins.dlyons.dev, and the DHCP
  # reservation for this machine's address. Those survive a garfield failure,
  # but must be re-pointed at whatever replaces it.

  systemd.services.gtm-backup = {
    description = "Back up n8n workflows and Home Assistant state to rev-ops";
    # Both sources must be up, or the run is pointless.
    after = [ "postgresql.service" "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = lib.getExe backupScript;
      # Root: reads /var/lib/hass, drops to postgres for the dump and to
      # ${user} for git (whose SSH key is what GitHub already trusts).
      User = "root";
    };
  };

  systemd.timers.gtm-backup = {
    description = "Nightly GTM and Home Assistant backup";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "*-*-* 03:30:00";
      # Catch up after downtime rather than silently skipping a night.
      Persistent = true;
      RandomizedDelaySec = "10m";
    };
  };
}
