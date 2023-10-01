<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Nix for MacOS + NixOS
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

# Overview
Hey, you made it! Welcome. 🤓 

This Nix configuration runs on MacOS, NixOS, or both simultaneously. It's also a good example of a MacOS Nix flake.

I use this daily on my 🧑🏻‍💻 M1 Macbook Pro and an x86 PC in my home office.

Check out the starter templates below to get started!

## Table of Contents
- [Nix for MacOS + NixOS](#nix-for-macos--nixos)
  - [Overview](#overview)
    - [Layout](#layout)
    - [Features](#features)
  - [Videos](#videos)
    - [MacOS](#macos)
      - [Updating dependencies with one command](#updating-dependencies-with-one-command)
      - [Instant Emacs 29 thanks to daemon mode](#instant-emacs-29-thanks-to-daemon-mode)
    - [NixOS](#nixos)
  - [Installing](#installing)
    - [For MacOS](#for-macos)
      - [1. Install dependencies](#1-install-dependencies)
      - [2. Install Nix](#2-install-nix)
      - [3. Initialize a starter template](#3-initialize-a-starter-template)
      - [4. Apply your current user info](#4-apply-your-current-user-info)
      - [5. Decide what packages to install](#5-decide-what-packages-to-install)
      - [6. Optional: setup secrets](#6-optional-setup-secrets)
      - [7. Install configuration](#7-install-configuration)
    - [For NixOS](#for-nixos)
      - [1. Burn the latest ISO](#1-burn-the-latest-iso)
      - [2. Optional: setup secrets](#2-optional-setup-secrets)
      - [3. Install configuration](#3-install-configuration)
      - [4. Set user password](#4-set-user-password)
  - [How to Create Secrets](#how-to-create-secrets)
  - [Live ISO](#live-iso)
  - [Making Changes](#making-changes)
    - [For MacOS](#for-macos-1)
    - [For NixOS](#for-nixos-1)
    - [Update dependencies](#update-dependencies)
  - [Appendix](#appendix)

## Layout
```
.
├── bin          # Optional scripts used to run build/update
├── shared       # Shared configurations applicable to all systems
├── darwin       # MacOS and nix-darwin configuration
├── nixos        # My NixOS desktop-related configuration
├── overlays     # Drop an overlay file in this dir, and it runs. So far, mainly patches.
├── templates    # Starter versions of this configuration
```

## Features
- **Nix Flakes**: 100% flake driven, no `configuration.nix`, no Nix channels─ just `flake.nix`
- **Same Environment Everywhere**: Easily share config across Linux and Mac (both Nix and Home Manager)
- **MacOS Dream Setup**: Fully declarative MacOS, including UI, dock and MacOS App Store apps
- **Simple Bootstrap**: Simple Nix commands to start from zero, both x86 and MacOS platforms
- **Managed Homebrew**: Fully managed homebrew environment with `nix-darwin` and `nix-homebrew`
- **Disk Management**: Declarative disk management with `disko`, say goodbye to disk utils
- **Secrets Management**: Declarative secrets with `agenix` for SSH, PGP, syncthing, and other tools
- **Super Fast Emacs**: Bleeding edge Emacs that fixes itself, thanks to a community overlay
- **Built In Home Manager**: `home-manager` module for seamless configuration (no extra clunky CLI steps)
- **NixOS Environment**: Extensively configured NixOS including clean aesthetic + window animations
- **Nix Overlays**: Auto-loading of Nix overlays: drop a file in a dir and it runs (great for patches!)
- **Declarative Sync**: No-fuss Syncthing: managed keys, certs, and configuration across all platforms
- **Emacs Literate Configuration**: Large Emacs literate configuration to explore (if that's your thing)
- **Simplicity and Readability**: Optimized for simplicity and readability in all cases, not small files everywhere

# Videos 
## MacOS
### Updating dependencies with one command
https://github.com/dustinlyons/nixos-config/assets/1292576/2168d482-6eea-4b51-adc1-2ef1291b6598

### Instant Emacs 29 thanks to daemon mode
**GUI**

https://github.com/dustinlyons/nixos-config/assets/1292576/66001066-2bbf-4492-bc9e-60ea1abeb987

**Terminal**

https://github.com/dustinlyons/nixos-config/assets/1292576/d96f59ce-f540-4f14-bc61-6126a74f9f52

## NixOS

https://github.com/dustinlyons/nixos-config/assets/1292576/fa54a87f-5971-41ee-98ce-09be048018b8

# Installing
## For MacOS
I've tested these instructions on a fresh Macbook Pro as of September 2023.

### 1. Install dependencies
```sh
xcode-select --install
```

### 2. Install Nix
Thank you for the installer, [Determinate Systems](https://determinate.systems/)!
```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### 3. Initialize a starter template
This is a simplified version without secrets management.
```sh
nix flake init -t github:dustinlyons/nixos-config#starter
```

This is a full version with secrets management.
```sh
nix flake init -t github:dustinlyons/nixos-config#starterWithSecrets
```

### 4. Apply your current user info
Run this script to replace stub values with your username, full name, and email.
```sh
chmod +x bin/apply && bin/apply
```

### 5. Decide what packages to install
You can search for packages on the [official NixOS website](https://search.nixos.org/packages).

**Review these files**

* `darwin/casks`
* `darwin/packages`
* `darwin/home-manager`
* `nixos/packages`
* `shared/packages`

### 6. Optional: Setup secrets
If you are using the starter with secrets, there are a few additional steps.

#### 6a. Create a private Github repo to hold your secrets
In Github, create a private `nix-secrets` repository. 

Then, change the `nix-secrets` input in the `flake.nix` to reference it.

#### 6b. Install keys
Before geneating your first build, these keys need to exist in your `~/.ssh` directory. I've provided a few helper commands below. Choose one.

| Key Name            | Platform         | Description                           | 
|---------------------|------------------|---------------------------------------|
| id_ed25519          | MacOS / NixOS    | Used to download secrets from Github. |
| id_ed25519_agenix   | MacOS / NixOS    | Used to encrypt and decrypt secrets.  |

##### Copy keys from USB drive
This script auto-detects a USB drive connected to the current system.
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
nix run github:dustinlyons/nixos-config#copyKeys
```

##### Create new keys
```sh
nix run github:dustinlyons/nixos-config#createKeys
```

##### Check existing keys
If you're rolling your own, just check they are installed correctly.
```sh
nix run github:dustinlyons/nixos-config#checkKeys
```

### 7. Install configuration
First-time installations require you to move the current `/etc/nix/nix.conf` out of the way.
```sh
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
```

Then, run this script, which wraps the Nix commands to build and deploy a new Nix generation.
```sh
chmod +x bin/darwin-build && chmod +x bin/build && bin/build
```

## For NixOS
### 1. Burn the latest ISO
Download and burn [the minimal ISO image](https://nixos.org/download.html). Boot the installer.

### 2. Optional: Setup secrets
If you are using the starter with secrets, there are a few additional steps.

#### 2a. Create a private Github repo to hold your secrets
In Github, create a private `nix-secrets` repository. 

Then, change the `nix-secrets` input in the `flake.nix` to reference it.

#### 2b. Install keys
Before geneating your first build, these keys need to exist in your `~/.ssh` directory. I've provided a few helper commands below. Choose one.

| Key Name            | Platform         | Description                           | 
|---------------------|------------------|---------------------------------------|
| id_ed25519          | MacOS / NixOS    | Used to download secrets from Github. |
| id_ed25519_agenix   | MacOS / NixOS    | Used to encrypt and decrypt secrets.  |

##### Copy keys from USB drive
This script auto-detects a USB drive connected to the current system.
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#copyKeys
```

##### Create new keys
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#createKeys
```

##### Check existing keys
If you're rolling your own, just check they are installed correctly.
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#checkKeys
```

### 3. Install configuration
#### Run command
After the keys are in place, you're good to go. Run either of these commands:

> [!IMPORTANT]
> For Nvidia cards, select the second option, `nomodeset`, when booting the installer.

> [!WARNING]
> Running this will reformat your drive to the ext4 filesystem.

**Simple**
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#install
```

**With secrets**
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#installWithSecrets
```

### 4. Set user password
On first boot at the login screen:
- Use the shortcut `Ctrl-Alt-F2` to move to a terminal session
- Login as `root` using the password created during installation
- Set the user password with `passwd <user>`
- Go back to the login screen: `Ctrl-Alt-F7`

# How to create secrets
To create a new secret `secret.age`, first [create a `secrets.nix` file](https://github.com/ryantm/agenix#tutorial) at the root of your `nix-secrets` repository. This is only used by the `agenix` CLI command. It assumes your SSH private key is in `~/.ssh/` or you can provide the `-i` flag with a path to your `id_ed25519` key.
```
let
  user1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0idNvgGiucWgup/mP78zyC23uFjYq0evcWdjGQUaBH";
  users = [ user1 ];

  system1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPJDyIr/FSz1cJdcoW69R+NrWzwGK/+3gJpqD1t8L2zE";
  systems = [ system1 ];
in
{
  "secret.age".publicKeys = [ user1 system1 ];
}
```
Then run this command: 
```
EDITOR=vim nix run github:ryantm/agenix -- -e secret.age
```
This opens an editor to accept, encrypt, and write your secret to disk. Commit the file to your `nix-secrets` repo and add a reference in the `secrets.nix` of your `nixos-config`.

## Secrets used in my configuration
| Secret Name           | Platform         | Description           | 
|-----------------------|------------------|-----------------------|
| `syncthing-cert`      | MacOS / NixOS    | Syncthing certificate |
| `syncthing-key`       | MacOS / NixOS    | Syncthing key         |
| `github-ssh-key`      | MacOS / NixOS    | GitHub SSH key        |
| `github-signing-key`  | MacOS / NixOS    | GitHub signing key    |

These are the secrets I use.

When changing secrets after your configuration exists, be sure to run `nix flake update` from your `nixos-config` so that you reference the latest change.

# Live ISO
Not yet available. Coming soon.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#live
```

# Making changes
With Nix, changes to your system are made by 
- editing your system configuration
- building the [system closure](https://zero-to-nix.com/concepts/closures)
- creating and switching to the [new generation](https://nixos.wiki/wiki/Terms_and_Definitions_in_Nix_Project#generation)

## For MacOS
```sh
nix build .#darwinConfigurations.macos.system && \
./result/sw/bin/darwin-rebuild switch --flake .#macos
```
#### Optional script to save keystrokes
```sh
bin/build
```
## For NixOS
```sh
sudo nixos-rebuild switch --flake .#nixos
```
#### Optional script to save keystrokes
```sh
bin/build
```

## Update dependencies
```sh
nix flake update
```

## Appendix
### NixOS Components

| Component                   | Description                                     | 
| --------------------------- | :---------------------------------------------  |
| **Window Manager**          | Xorg + bspwm                                    |
| **Terminal Emulator**       | alacritty                                       |
| **Bar**                     | polybar                                         |
| **Application Launcher**    | rofi                                            |
| **Notification Daemon**     | dunst                                           |
| **Display Manager**         | lightdm                                         |
| **File Manager**            | thunar                                          |
| **Text Editor**             | emacs daemon mode                               |
| **Media Player**            | cider                                           |
| **Image Viewer**            | feh                                             |
| **Screenshot Software**     | flameshot                                       |

### Stars

> "All we have to decide is what to do with the time that is given us." - J.R.R. Tolkien

[![Star History Chart](https://api.star-history.com/svg?repos=dustinlyons/nixos-config&type=Date)](https://star-history.com/#dustinlyons/nixos-config&Date)

[![Follow @dustinhlyons](https://github.com/dustinlyons/dustinlyons/assets/1292576/3d214b95-6c93-4967-8c72-862fa494e664)](https://www.twitter.com/dustinhlyons)
