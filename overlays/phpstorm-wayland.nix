final: prev: {
  jetbrains = prev.jetbrains // {
    phpstorm = prev.jetbrains.phpstorm.overrideAttrs (oldAttrs: {
      postFixup = (oldAttrs.postFixup or "") + ''
        # Enable native Wayland support using WLToolkit
        wrapProgram $out/bin/phpstorm \
          --add-flags "-Dawt.toolkit.name=WLToolkit"
      '';
    });
  };
}