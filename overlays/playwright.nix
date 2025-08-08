# Playwright browser dependencies overlay
final: prev: 
let
  playwrightLibs = with prev; [
    glib
    gtk3
    nss
    nspr
    dbus
    at-spi2-atk
    at-spi2-core
    cups
    libdrm
    mesa
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    xorg.libxcb
    cairo
    pango
    expat
    libxkbcommon
    systemd
    alsa-lib
    freetype
    fontconfig
    gdk-pixbuf
    libnotify
    liberation_ttf
  ];
in
{
  playwright-deps = prev.buildEnv {
    name = "playwright-deps";
    paths = with prev; [
      chromium
    ] ++ playwrightLibs;
  };

  playwright-wrapper = prev.writeScriptBin "playwright-wrapper" ''
    #!${prev.bash}/bin/bash
    # Playwright wrapper script for cron jobs and system-wide usage

    # Playwright configuration
    export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
    export PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS=true
    export PLAYWRIGHT_CHROMIUM_EXECUTABLE_PATH="${prev.chromium}/bin/chromium"
    export PLAYWRIGHT_BROWSERS_PATH=0
    export PLAYWRIGHT_SKIP_BROWSER_GC=1

    # Set up library path
    export LD_LIBRARY_PATH="${prev.lib.makeLibraryPath playwrightLibs}:$LD_LIBRARY_PATH"

    # Execute the command passed to this wrapper
    exec "$@"
  '';
}