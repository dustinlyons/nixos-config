<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Nix for MacOS + NixOS
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

_Psst: I can help make your dev environments easy to use and match production. <a href="https://calendly.com/dustinhlyons/business-intro-call">Get in touch.</a>_

# Overview
Hey, you made it! Welcome. 🤓 

This Nix configuration runs on MacOS, NixOS, or both simultaneously. It's also a good example of a MacOS Nix flake.

I use this daily on my 🧑🏻‍💻 M1 Macbook Pro and an x86 PC in my home office.

## Layout
```
.
├── bin          # Optional scripts used to run build/update
├── shared       # Shared configurations applicable to all systems
├── darwin       # MacOS and nix-darwin configuration
├── nixos        # My NixOS desktop-related configuration
├── overlays     # Drop an overlay file in this dir, and it runs. So far, mainly patches.
└── vms          # VM-specific configs running in my home-lab
```

## Features
- **Same Environment Everywhere**: Easily share config across Linux and Mac (both Nix and Home Manager)
- **MacOS Dream Setup**: Fully declarative MacOS, including UI, dock and MacOS App Store apps
- **Bootstrap Nix Command**: Simple `nix-command` to start from zero, both x86 and MacOS platforms
- **Managed Homebrew**: Fully managed, auto-updating homebrew environment with `nix-darwin`
- **Disk Management**: Declarative disk management with `disko`, say goodbye to disk utils
- **Secrets Management**: Declarative secrets with `agenix` for SSH, PGP, syncthing, and other tools
- **Super Fast Emacs**: Bleeding edge Emacs that fixes itself, thanks to a community overlay
- **Nix Flakes**: _Almost_ 100% flake driven, no major use of channels or `configuration.nix`
- **Built In Home Manager**: Home-manager module for seamless configuration (no extra clunky CLI steps)
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
### Create a private Github repo to hold your secrets
In Github, create a private `nix-secrets` repository. Later, I show you how to create private data that lives there.

### Install dependencies
```sh
xcode-select --install
```

### Install Nix
```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### Clone this flake
```sh
nix flake clone github:dustinlyons/nixos-config
```

### Install some keys
Pick your poison.

**Copy keys from USB drive**
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
nix run github:dustinlyons/nixos-config#copyKeys
```
**Create new keys**
```sh
nix run github:dustinlyons/nixos-config#createKeys
```
If you want to bring keys from somewhere else, make sure they're named correctly.

**Check existing keys**
```sh
nix run github:dustinlyons/nixos-config#checkKeys
```

### Install configuration
First-time installations require you to move the current `/etc/nix/nix.conf` out of the way.
```sh
sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
```

Then, run this script, which wraps the Nix commands to build and deploy a new generation.
```sh
./bin/build
```
Use this same script after making edits to the configuration.

## For NixOS
### Create a private Github repo to hold your secrets
In Github, create a private `nix-secrets` repository. Later, I show you how to create private data that lives there.

### Burn the latest ISO
Download and burn [the minimal ISO image](https://nixos.org/download.html). Boot the installer.

### Install some keys
With the installer running, do one of these:

**Copy keys from USB drive**
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#copyKeys
```
**Create new keys**
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#createKeys
```
If you want to bring keys from somewhere else, make sure they're named correctly.

**Check existing keys**
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#checkKeys
```

### Install configuration
#### Run command
After the keys are in place, you're good to go. Run this command:

> [!IMPORTANT]
> For Nvidia cards, select the second option, `nomodeset`, when booting the installer.

> [!WARNING]
> Running this will reformat your drive to the ext4 filesystem.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#install
```
### Set user password
On first boot at the login screen:
- Use the shortcut `Ctrl-Alt-F2` to move to a terminal session
- Login as `root` using the password created during installation
- Set the user password with `passwd <user>`
- Go back to the login screen: `Ctrl-Alt-F7`

### How to create secrets
To create a new secret `secret.age`, first [create a `secrets.nix` file](https://github.com/ryantm/agenix#tutorial) at the root of your `nix-secrets` repository. This is only used by the `agenix` CLI command. It assumes your SSH private key is in `~/.ssh/` or you can provide the `-i` flag with a path to your `id_ed25519_agenix` key.
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
This will create a `secret.age` file with your secret that you can reference in the Nix configuration. Commit the file to your repo.

### Secrets used in this configuration
| Secret Name           | Platform         | Description           | 
|-----------------------|------------------|-----------------------|
| `syncthing-cert`      | MacOS / NixOS    | Syncthing certificate |
| `syncthing-key`       | MacOS / NixOS    | Syncthing key         |
| `github-ssh-key`      | MacOS / NixOS    | GitHub SSH key        |
| `github-signing-key`  | MacOS / NixOS    | GitHub signing key    |

When changing secrets after your configuration exists, be sure to run `nix flake update` from your `nixos-config` so that you reference the latest change.

## Live ISO
Not yet available. Coming soon.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#live
```

## Emacs
When running `emacs` for the first time, install `all-the-icons`.
```
M-x all-the-icons-install-fonts
```

# Making changes
With Nix, changes to your system are made by 
- editing your system configuration
- building it
- switching your running system to use it

## For MacOS
```sh
nix --experimental-features 'nix-command flakes' build .#darwinConfigurations.Dustins-MBP.system --impure && \
./result/sw/bin/darwin-rebuild switch --flake .#Dustins-MBP --impure && \
unlink ./result
```
## For NixOS
```sh
sudo nixos-rebuild switch --flake .#felix
```
#### Optional script to save keystrokes
```sh
./bin/build
```

## Update dependencies
```sh
nix flake update
```

## Read my occasional musings on Nix
[![Follow @dustinhlyons](https://github.com/dustinlyons/dustinlyons/assets/1292576/3d214b95-6c93-4967-8c72-862fa494e664)](https://www.twitter.com/dustinhlyons)

> "All we have to decide is what to do with the time that is given us." - J.R.R. Tolkien

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=dustinlyons/nixos-config&type=Date)](https://star-history.com/#dustinlyons/nixos-config&Date)
