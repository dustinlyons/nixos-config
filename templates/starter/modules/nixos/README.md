## Layout
```
.
├── config             # Config files not written in Nix
├── default.nix        # Defines module, system-level config,
├── disk-config.nix    # Disks, partitions, and filesystems
├── files.nix          # Non-Nix, static configuration files (now immutable!)
├── home-manager.nix   # Defines user programs
├── packages.nix       # List of packages to install for NixOS
```

## Multiple Host Support

By default, the configuration provides one host per platform (x86_64-linux, aarch64-linux) that auto-detects your system architecture. To add additional named hosts with different configurations:

### 1. Create host-specific directories
```bash
mkdir -p hosts/nixos/hostname
```

### 2. Add to flake.nix
In your `nixosConfigurations`, add named hosts alongside the existing platform-based ones:
```nix
nixosConfigurations = 
  # Platform-based (existing default hosts)
  nixpkgs.lib.genAttrs linuxSystems (system:
    nixpkgs.lib.nixosSystem {
      inherit system;
      specialArgs = inputs // { inherit user; };
      modules = [
        # existing modules...
        ./hosts/nixos
      ];
    }
  )
  
  // # Additional named hosts
  {
    hostname = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = inputs // { inherit user; };
      modules = [
        # Same base modules...
        ./hosts/nixos/hostname  # Host-specific config
      ];
    };
  };
```

### 3. Create host configuration
```nix
# hosts/nixos/hostname/default.nix
{ config, lib, pkgs, ... }:
{
  imports = [
    ./hardware-configuration.nix  # Generate with nixos-generate-config
    ../../../modules/shared       # Shared base configuration
    # Add/remove modules as needed
  ];
  
  networking.hostName = "hostname";
  # Host-specific overrides (hardware drivers, packages, services)...
}
```

### 4. Build and deploy
```bash
# Build default host (auto-detects platform)
nix run .#build-switch

# Build specific named host
nix run .#build-switch -- --host hostname
```

The `--host` flag allows targeting specific named hosts instead of the default platform-based configuration, enabling per-host customization while sharing the base configuration.

**Example:** Check out the `garfield` host in the [main repository](https://github.com/dustinlyones/nixos-config) to see how it differs from the default configuration with Nvidia graphics, different packages, simplified services, etc.

## Essential Hotkeys

After your first boot, here are the essential hotkeys to get started with the bspwm window manager:

### Core Navigation
- **Super + Space** - Open application launcher (rofi)
- **Super + Enter** - Open terminal (floating)
- **Super + Ctrl + Enter** - Open terminal (tiled)
- **Alt + F4** - Close window
- **Ctrl + Alt + Backspace** - Lock screen

### Window Management
- **Super + h/j/k/l** - Focus window (left/down/up/right)
- **Super + Shift + h/j/k/l** - Move window (left/down/up/right)
- **Super + f** - Toggle fullscreen
- **Super + d** - Toggle floating/tiled mode
- **Super + m** - Toggle monocle layout

### Workspaces
- **Super + 1-6** - Switch to workspace 1-6
- **Super + Shift + 1-6** - Move window to workspace 1-6
- **Super + Left/Right** - Switch to prev/next occupied workspace
- **Super + Tab** - Switch to last workspace

### Applications
- **Super + Alt + Enter** - Open Emacs
- **Ctrl + Alt + Enter** - Open web browser
- **Super + Shift + Space** - Open file manager
- **Super + Shift + x** - Open KeePassXC password manager
- **Print** - Take screenshot

### Audio Controls
- **XF86AudioRaiseVolume** - Volume up
- **XF86AudioLowerVolume** - Volume down  
- **XF86AudioMute** - Toggle mute

These hotkeys are defined in `~/.config/sxhkd/sxhkdrc` and can be customized by editing the configuration in this repository.
