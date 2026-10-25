{ config, pkgs, lib, ... }:

# ========================================
# Game-dev / 3D pipeline: Meshy → Blender → Unity
# ========================================
# Machine-level prerequisites for turning concept art into an interactive 3D
# world (the Atlas "company OS" game). Meshy AI is a hosted REST API, so it
# needs no package here — only an API key in ~/.secrets/meshy-api-key, read the
# same way the Atlas orchestrator reads its other secrets. Blender comes from
# nixpkgs. Unity is the awkward one on NixOS, and is handled in three layers:
#
#   1. pkgs.unityhub — the Hub GUI, wrapped by nixpkgs in an FHS sandbox
#      (bubblewrap presenting a conventional /usr, /lib layout) because Unity's
#      binaries assume a normal distro. Editors it installs land in
#      ~/Unity/Hub/Editor/<version>/ and inherit the sandbox when launched
#      from the Hub.
#   2. pkgs.unity-cli (overlays/unity-cli.nix) — Unity's new standalone
#      terminal CLI. Installs Editors/modules, creates and opens projects,
#      emits JSON. This is what agents drive. It runs inside the Hub's FHS
#      sandbox too (it probes /usr/lib/… for sqlite by fixed path), so an
#      Editor it opens inherits the same known-good environment as the Hub.
#   3. programs.nix-ld — provides the /lib64/ld-linux-x86-64.so.2 loader and a
#      library set so that ordinary Linux ELF binaries Unity drops outside Nix
#      (Editor, bee backend, il2cpp, bug reporter) can also start when launched
#      directly from a shell or a script rather than via the Hub/CLI sandbox.
#      The library list mirrors what nixpkgs puts in the unityhub FHS env.
#
# If `unity open .` still dies on a missing library, don't add random libs
# here first; run the Editor inside the same sandbox the Hub uses:
#   unity-fhs "$HOME/Unity/Hub/Editor/<ver>/Editor/Unity" -projectPath "$PWD"
#
# Nothing in this module manages Editor versions or Android/iOS toolchains;
# the Unity CLI owns those (`unity install lts`, `unity install-modules …`) so
# they stay mutable in ~/Unity while the prerequisites stay declarative.

let
  # Run any command inside the Unity Hub FHS sandbox from nixpkgs.
  unity-fhs = pkgs.writeShellScriptBin "unity-fhs" ''
    exec ${pkgs.unityhub.fhsEnv}/bin/unityhub-fhs-env "$@"
  '';

  # Blender from the binary cache. Cycles GPU rendering on the RX 9070 (RDNA 4)
  # would need `pkgs.blender.override { rocmSupport = true; }`, which is not
  # cached and compiles Blender + ROCm locally for hours. Modelling, cleanup
  # and export (the Meshy → Unity path) don't need it, so it stays off.
  blender = pkgs.blender;
in
{
  # Unity's own tooling and the Hub are unfree; the host already sets
  # nixpkgs.config.allowUnfree = true, restated here so the module is honest
  # about what it needs if it is ever imported elsewhere.
  nixpkgs.config.allowUnfree = true;

  # Loader + libraries for foreign ELF binaries (Unity Editor and helpers).
  programs.nix-ld = {
    enable = true;
    libraries = with pkgs; [
      # Hub / Electron-style deps
      cups gtk3 expat libxkbcommon krb5 alsa-lib nss nspr libdrm libgbm atk
      dbus at-spi2-core pango cairo
      xorg.libXcomposite xorg.libXext xorg.libXdamage xorg.libXfixes
      xorg.libxcb xorg.libxshmfence xorg.libXScrnSaver xorg.libXtst
      xorg.libXrandr xorg.libXi xorg.libXrender xorg.libXcursor xorg.libX11
      libva openssl libnotify libuuid libsecret udev libappindicator wayland
      icu libpulseaudio
      # Editor deps
      libglvnd glib gdk-pixbuf libxml2 zlib libcap harfbuzz vulkan-loader
      fontconfig freetype
      stdenv.cc.cc.lib
    ];
  };

  # `~/.local/bin` on PATH for every login shell. Unity's own install.sh puts
  # the CLI there; we ship it via Nix instead, but anything else Unity or a
  # script drops there (Hub CLI shims, per-user tools) stays reachable.
  environment.localBinInPath = true;

  environment.systemPackages = with pkgs; [
    unityhub    # Unity Hub GUI in its FHS sandbox (Editor installs + licensing UI)
    unity-cli   # `unity` — Unity's terminal CLI (overlays/unity-cli.nix)
    unity-fhs   # run an arbitrary command inside the Hub's FHS sandbox
    blender     # 3D cleanup / retopo / material fixes between Meshy and Unity
    git-lfs     # Unity projects keep binary assets in LFS
    p7zip       # Unity module archives
  ];
}
