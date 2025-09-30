{ config, pkgs, lib, inputs, ... }:

let
  user = "dustin";
  xdg_configHome  = "/home/${user}/.config";
  shared-programs = import ../shared/home-manager.nix { inherit config pkgs lib; };
  shared-files = import ../shared/files.nix { inherit config pkgs; };
  kde-config = import ./kde-config.nix;

  # These files are generated when secrets are decrypted at build time
  gpgKeys = [
    "/home/${user}/.ssh/pgp_github.key"
    "/home/${user}/.ssh/pgp_github.pub"
  ];
in
{

  home = {
    enableNixpkgsReleaseCheck = false;
    username = "${user}";
    homeDirectory = "/home/${user}";
    packages = pkgs.callPackage ./packages.nix { inherit inputs config; };
    file = shared-files // import ./files.nix { inherit user pkgs; };
    stateVersion = "25.05";
    
    # Playwright environment variables for NixOS
    sessionVariables = {
      PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD = "1";
      PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = "true";
      PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH = "${pkgs.chromium}/bin/chromium";
    };
  };

  programs = shared-programs // { 
    gpg.enable = true;
    
    rofi = {
      enable = true;
      package = pkgs.rofi;
      theme = let
        inherit (config.lib.formats.rasi) mkLiteral;
      in {
        "*" = {
          # Plasma 6 Breeze Dark color scheme
          background = mkLiteral "#1e1e2e";
          background-alt = mkLiteral "#252536";
          foreground = mkLiteral "#eff1f5";
          selected = mkLiteral "#3daee9";
          selected-foreground = mkLiteral "#1e1e2e";
          active = mkLiteral "#7aa2f7";
          urgent = mkLiteral "#f38ba8";
          border-color = mkLiteral "#31363b";
          
          border-radius = mkLiteral "6px";
          font = "Inter 11";
        };
        
        "window" = {
          transparency = "real";
          background-color = mkLiteral "@background";
          text-color = mkLiteral "@foreground";
          border = mkLiteral "1px";
          border-color = mkLiteral "@border-color";
          border-radius = mkLiteral "8px";
          width = mkLiteral "650px";
          location = mkLiteral "center";
          x-offset = 0;
          y-offset = 0;
          padding = mkLiteral "2px";
        };
        
        "mainbox" = {
          background-color = mkLiteral "@background";
          border = mkLiteral "0";
          padding = mkLiteral "0";
        };
        
        "inputbar" = {
          children = mkLiteral "[ prompt,textbox-prompt-colon,entry,case-indicator ]";
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          expand = false;
          border = mkLiteral "0px 0px 1px 0px";
          border-radius = mkLiteral "6px 6px 0px 0px";
          border-color = mkLiteral "@border-color";
          margin = mkLiteral "0px";
          padding = mkLiteral "12px";
        };
        
        "prompt" = {
          enabled = true;
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@selected";
        };
        
        "textbox-prompt-colon" = {
          expand = false;
          str = ":";
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
        };
        
        "entry" = {
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          placeholder-color = mkLiteral "@foreground";
          expand = true;
          horizontal-align = mkLiteral "0";
          placeholder = "Search...";
          padding = mkLiteral "0px 0px 0px 8px";
          blink = true;
        };
        
        "case-indicator" = {
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          spacing = mkLiteral "0";
        };
        
        "listview" = {
          background-color = mkLiteral "@background";
          columns = 1;
          lines = 10;
          spacing = mkLiteral "8px";
          cycle = true;
          dynamic = true;
          layout = mkLiteral "vertical";
          padding = mkLiteral "8px";
        };
        
        "element" = {
          background-color = mkLiteral "@background";
          text-color = mkLiteral "@foreground";
          orientation = mkLiteral "horizontal";
          border-radius = mkLiteral "6px";
          padding = mkLiteral "8px 12px";
          spacing = mkLiteral "8px";
        };
        
        "element-icon" = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
          size = mkLiteral "24px";
          border = mkLiteral "0px";
        };
        
        "element-text" = {
          background-color = mkLiteral "inherit";
          text-color = mkLiteral "inherit";
          expand = true;
          horizontal-align = mkLiteral "0";
          vertical-align = mkLiteral "0.5";
          margin = mkLiteral "0px 2.5px 0px 2.5px";
        };
        
        "element selected" = {
          background-color = mkLiteral "@selected";
          text-color = mkLiteral "@selected-foreground";
          border = mkLiteral "0px";
          border-radius = mkLiteral "6px";
        };
        
        "element alternate" = {
          background-color = mkLiteral "@background";
          text-color = mkLiteral "@foreground";
        };
        
        "mode-switcher" = {
          enabled = true;
          background-color = mkLiteral "@background-alt";
          expand = false;
          border = mkLiteral "1px 0px 0px 0px";
          border-radius = mkLiteral "0px 0px 6px 6px";
          border-color = mkLiteral "@border-color";
          padding = mkLiteral "12px";
          spacing = mkLiteral "8px";
        };
        
        "button" = {
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          cursor = mkLiteral "pointer";
          padding = mkLiteral "8px 12px";
          border-radius = mkLiteral "6px";
        };
        
        "button selected" = {
          background-color = mkLiteral "@selected";
          text-color = mkLiteral "@selected-foreground";
        };
        
        "message" = {
          enabled = true;
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          border = mkLiteral "1px 0px 0px 0px";
          border-radius = mkLiteral "0px 0px 6px 6px";
          border-color = mkLiteral "@border-color";
          padding = mkLiteral "12px";
        };
        
        "textbox" = {
          background-color = mkLiteral "@background-alt";
          text-color = mkLiteral "@foreground";
          vertical-align = mkLiteral "0.5";
          horizontal-align = mkLiteral "0.0";
        };
      };
      extraConfig = {
        show-icons = true;
        icon-theme = "breeze-dark";
        display-drun = "Applications";
        display-run = "Run";
        display-window = "Windows";
        drun-display-format = "{name}";
        disable-history = false;
        hide-scrollbar = true;
        sidebar-mode = true;
        matching = "fuzzy";
        sort = true;
      };
    };
    plasma = lib.recursiveUpdate kde-config.programs.plasma {
      hotkeys.commands = {
        "view-cheatsheets" = {
          name = "View Cheatsheets";  
          key = "Meta+C";
          command = "cheatsheet-viewer";
        };
        "zeditor" = {
          name = "Zed Editor";
          key = "Meta+Z";
          command = "zeditor";
        };
      };
      
      workspace = {
        clickItemTo = "select";
      };
    };
  };

  # This installs my GPG signing keys for Github
  systemd.user.services.gpg-import-keys = {
    Unit = {
      Description = "Import gpg keys";
      After = [ "gpg-agent.socket" ];
    };

    Service = {
      Type = "oneshot";
      ExecStart = toString (pkgs.writeScript "gpg-import-keys" ''
        #! ${pkgs.runtimeShell} -el
        ${lib.optionalString (gpgKeys != []) ''
        ${pkgs.gnupg}/bin/gpg --import ${lib.concatStringsSep " " gpgKeys}
        ''}
      '');
    };

    Install = { WantedBy = [ "default.target" ]; };
  };
}
