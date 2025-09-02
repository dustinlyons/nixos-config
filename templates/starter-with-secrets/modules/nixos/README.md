## Layout
```
.
├── config             # Config files not written in Nix
├── default.nix        # Defines module, system-level config,
├── disk-config.nix    # Disks, partitions, and filesystems
├── files.nix          # Non-Nix, static configuration files (now immutable!)
├── home-manager.nix   # Defines user programs
├── packages.nix       # List of packages to install for NixOS
├── secrets.nix        # Age-encrypted secrets with agenix
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
