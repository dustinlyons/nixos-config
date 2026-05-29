# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a cross-platform NixOS configuration repository supporting both macOS (via nix-darwin) and NixOS systems. It uses Nix Flakes exclusively and follows a modular architecture.

## System Environment

- **Display Server**: Wayland (not X11)
- **Desktop Environment**: KDE Plasma 6
- **Window Manager**: KWin (Wayland)

Important: When working with GUI applications, ensure Wayland compatibility. For example:
- Use `rofi-wayland` instead of `rofi`
- Check for Wayland-specific versions of applications
- Some X11-only applications may need XWayland compatibility layer

## Key Commands

### Building and Switching Configurations

**NixOS (x86_64-linux):**
```bash
# Build and switch to new configuration
nix run .#build-switch

# Build, switch, and restart Emacs daemon (use after Emacs config changes)
nix run .#build-switch-emacs

# Install fresh system (without secrets)
nix run .#install

# Install fresh system (with secrets)
nix run .#install-with-secrets

# Clean up old generations and boot files (frees disk space)
nix run .#clean
```

**Important:** 
- After making changes to Nix configuration files, run `nix run .#build-switch`
- After making changes to Emacs configuration (`modules/shared/config/emacs/config.org`), run `nix run .#build-switch-emacs`

**macOS (x86_64-darwin):**
```bash
# Test build without switching
nix run .#build

# Build and switch to new configuration
nix run .#build-switch

# Rollback to previous generation
nix run .#rollback

# Clean up old generations (frees disk space)
nix run .#clean
```

### Development Commands

```bash
# Update flake inputs
nix flake update

# Check flake
nix flake check

# Format Nix files
nixpkgs-fmt .

# Lint Nix code (runs in CI)
statix check
```

## Architecture

### Directory Structure
- `hosts/` - Host-specific configurations (darwin/default.nix for macOS, nixos/default.nix for NixOS)
- `modules/` - Modular configuration components:
  - `shared/` - Cross-platform configurations (packages, home-manager, fonts)
  - `nixos/` - Linux-specific packages and configurations
  - `darwin/` - macOS-specific packages and Homebrew integration
- `overlays/` - Auto-loading Nix overlays (any .nix file here runs automatically)
- `apps/` - Platform-specific build and deployment scripts
- `templates/` - Starter templates for new users

### Key Patterns

1. **Module Inheritance**: Platform-specific modules extend shared configurations
   ```nix
   # Example from modules/nixos/packages.nix
   packages = with pkgs; [
     # Shared packages are inherited
   ] ++ sharedPackages;
   ```

2. **Auto-loading Overlays**: Drop any .nix file in `overlays/` and it loads automatically via:
   ```nix
   # modules/shared/default.nix
   nixpkgs.overlays = import ../../overlays {inherit lib pkgs inputs outputs;};
   ```

3. **Secrets Management**: Uses `agenix` for encrypted secrets
   - Secrets defined in `hosts/{platform}/secrets/secrets.nix`
   - Age keys in `hosts/{platform}/secrets/keys/`

4. **Home Manager Integration**: User-level configurations in `modules/shared/home-manager.nix`

### Important Configuration Files

- `flake.nix` - Main entry point defining inputs and system configurations
- `hosts/nixos/default.nix` - NixOS system configuration (hostname: "felix")
- `hosts/darwin/default.nix` - macOS system configuration
- `modules/shared/packages.nix` - Cross-platform package definitions
- `modules/shared/home-manager.nix` - Shell, editor, and tool configurations

## Working with This Repository

### CRITICAL: Git Tracking Requirement
**IMPORTANT:** When creating ANY new file in this repository (overlays, modules, configurations, etc.), you MUST add it to git with `git add` before running `nix run .#build-switch`. Nix flakes only see files tracked by git, so untracked files will cause build failures. This applies to ALL files, not just overlays.

### Adding Packages
1. **Cross-platform packages**: Add to `modules/shared/packages.nix`
2. **NixOS-only packages**: Add to `modules/nixos/packages.nix`
3. **macOS-only packages**: Add to `modules/darwin/packages.nix`
4. **Homebrew casks (macOS)**: Add to `modules/darwin/casks.nix`

### Creating Overlays
Create a new .nix file in `overlays/` directory. It will be automatically loaded.

### Modifying Shell Configuration
Edit `modules/shared/home-manager.nix` for:
- Zsh configuration and aliases
- Git settings
- Terminal emulator (Alacritty) configuration
- Tmux settings
- SSH configuration

### Editing Emacs Configuration
The Emacs configuration is a literate Org-mode file at `modules/shared/config/emacs/config.org`. This file:
- Contains all Emacs configuration in Org-mode code blocks
- Is automatically tangled to `config.el` when building
- Includes comprehensive documentation alongside the configuration
- Supports adding new packages and customizations in the appropriate sections

**Important:** When making changes to Emacs configuration:
1. Edit `modules/shared/config/emacs/config.org`
2. If adding a new package, also add it to `modules/shared/emacs.nix`
3. Run `nix run .#build-switch` to rebuild the configuration
4. Restart the Emacs daemon with: `emacsclient -e "(kill-emacs)"`
5. The daemon will automatically restart when you next use Emacs
6. **Update cheatsheets:** Check if the relevant cheatsheets in `/home/dustin/cheatsheets/` need to be updated with new keybindings or features (especially `emacs.md`)

### System-specific Changes
- **NixOS boot/kernel**: `hosts/nixos/default.nix`
- **macOS system settings**: `hosts/darwin/default.nix`
- **Disk partitioning (NixOS)**: `hosts/nixos/disk-config.nix`

## Testing Changes

Always test configuration changes before applying:
1. Run `nix flake check` to validate the flake
2. Use platform-specific build command to test
3. Review changes before switching

## CI/CD

GitHub Actions automatically:
- Tests starter template builds
- Runs statix linting on Nix code
- Updates flake.lock weekly
- Manages dependencies via Dependabot