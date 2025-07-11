final: prev: {
  jetbrains = prev.jetbrains // {
    phpstorm = prev.jetbrains.phpstorm.overrideAttrs (oldAttrs: {
      postFixup = (oldAttrs.postFixup or "") + ''
        # Create a wrapper that sets environment for Wayland compatibility
        # Use XWayland with proper window manager hints
        wrapProgram $out/bin/phpstorm \
          --set _JAVA_AWT_WM_NONREPARENTING 1 \
          --run 'export DISPLAY="''${DISPLAY:-:0}"'
      '';
    });
  };
}