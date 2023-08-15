<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Nix for MacOS + NixOS
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

_Psst: I can help bring Nix into your company. <a href="https://calendly.com/dustinhlyons/business-intro-call">Get in touch.</a>_

# Overview
Hey, you made it! Welcome. 🤓 

This Nix configuration runs on MacOS, NixOS, or both simultaneously. It's also a good example of a MacOS Nix flake.

I use this daily on my 🧑🏻‍💻 M1 Macbook Pro and an x86 PC in my home office.

## Layout
```
.
├── bin          # Optional scripts used to run build/update
├── common       # Shared configurations applicable to all systems
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
- **Disk Management**: Declarative disk management with Disko, say goodbye to disk utils
- **Secrets Management**: Declarative secrets with agenix for SSH, PGP, syncthing, and other tools
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

# Bootstrap New Computer
## Create a private secrets repository
This configuration assumes you have a private `nix-secrets` repository that holds `age`-encrypted files. These secrets are later read by `agenix`.

### Encrypting a secret
To create a new secret `secret.age`, first [create a `secrets.nix` file](https://github.com/ryantm/agenix#tutorial) at the root of your `nix-secrets` repository:
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
Then run this command and commit the `age` encrypted file:
```
EDITOR=vim nix run github:ryantm/agenix -- -e secret.age
```
> The key used for encryption must be available during installation. [Jump to these instructions](https://github.com/dustinlyons/nixos-config/blob/main/README.md#install-secrets).

### Active secrets
| Secret Name           | Platform | Description           | 
|-----------------------|----------|-----------------------|
| `syncthing-cert`      | MacOS    | Syncthing certificate |[
| `syncthing-key`       | MacOS    | Syncthing key         |
| `github-ssh-key`      | MacOS    | GitHub SSH key        |
| `github-signing-key`  | MacOS    | GitHub signing key    |
| `syncthing-cert`      | NixOS    | Syncthing certificate |
| `syncthing-key`       | NixOS    | Syncthing key         |
| `github-ssh-key`      | NixOS    | GitHub SSH key        |
| `github-signing-key`  | NixOS    | GitHub signing key    |

## Fork this repository and change it
You'll need to quickly scan files for where I've defined `user` at the top and change it to your username. 

You'll also likely want to change the name of the MacOS Nix flake target, packages, homebrew casks I install, and my Home Manager configuration. 
Make this repository your own and open a Github Issue if you have questions.

## For MacOS
## Install dependencies
```sh
xcode-select --install
```
```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```
```sh
nix run nix-darwin -- switch --flake ~/.config/nix-darwin
```
We still need this for the Home Manager `nix-darwin` module.
```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
```
```
nix-channel update
```
## Install config
```sh
nix --experimental-features 'nix-command flakes' build .#darwinConfigurations.Dustins-MBP.system --impure && \
./result/sw/bin/darwin-rebuild switch --flake .#Dustins-MBP --impure && \
unlink ./result
```
## For NixOS
## Burn the latest ISO
Download and burn [the minimal ISO image](https://nixos.org/download.html).

## Install secrets
This configuration assumes you have two [Ed25519 public and private key pairs](https://statistics.berkeley.edu/computing/ssh-keys) available on a USB drive that has been connected to the system.
* id_ed25519_agenix
* id_ed25519_agenix.pub
* id_ed25519_github
* id_ed25519_github.pub

`id_ed25519_agenix` is copied over and used to encrypt/decrypt `agenix` secrets. I use `id_ed25519_github` for my Github account.

Both are needed at install time to download my private `nix-secrets` Github repository and decrypt the configuration.

I keep these secrets `age`-encrypted with my Yubikey on two USB drives and decrypt them temporarily when bootstrapping a new system. You should either create your own keys and name them exactly as I have or fork this repo and change how my `secrets` `nix-command` handles the import (using KMS, CKM, paperkey, Hashicorp Vault, etc.). It's pretty simple `bash`.

Finally, plug in the USB drive, boot the installer and run this command:
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#secrets
```

## Install configuration
After the keys are in place, you're good to go. Just run this installation command.

> [!IMPORTANT]
> For Nvidia cards, select the second option `nomodeset` when booting the installer.

> [!WARNING]
> Running this will reformat your drive to the ext4 filesystem.
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#install
```

On first boot at the login screen:
- Use the shortcut `Ctrl-Alt-F2` to move to a terminal session
- Login as `root` using the password created during installation
- Set the user password with `passwd <user>`
- Go back to the login screen: `Ctrl-Alt-F7`

# Live ISO
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
