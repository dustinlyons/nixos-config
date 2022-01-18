self: super: {
    alacritty = super.alacritty.overrideAttrs (
      o: rec {
        doCheck = false;
      }
    );
  }
