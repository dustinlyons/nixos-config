{ config, pkgs, lib, ... }:
{
  systemd.user.services.atlas-devenv = {
    description = "Start atlas server in tmux";
    wantedBy = [ "default.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      Type = "forking";
      ExecStart = "${pkgs.writeShellScript "start-atlas" ''
        ATLAS_DIR=/home/dustin/.local/share/src/atlas

        # Install dependencies if needed
        if [ ! -d "$ATLAS_DIR/node_modules" ]; then
          cd "$ATLAS_DIR" && ${pkgs.nix}/bin/nix develop /home/dustin/.local/share/src/conductly --impure -c bash -c "bun install"
        fi

        cd /home/dustin/.local/share/src/conductly
        export TMUX_TMPDIR=/run/user/1000
        ${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-atlas new-session -d -s atlas "${pkgs.nix}/bin/nix develop --impure -c bash -c \"bun run $ATLAS_DIR/server.ts\""
      ''}";
      ExecStop = "${pkgs.tmux}/bin/tmux -S /run/user/1000/tmux-atlas kill-session -t atlas";
      RemainAfterExit = "no";
      Environment = [
        "PATH=/run/current-system/sw/bin:/home/dustin/.nix-profile/bin:/etc/profiles/per-user/dustin/bin:/nix/var/nix/profiles/default/bin:/run/wrappers/bin:/usr/bin:/bin"
      ];
    };
  };
}
