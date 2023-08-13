<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

_Psst: I can help write Nix at your company. <a href="https://twitter.com/dustinhlyons">Get in touch.</a>_
# Overview
Hey, you made it! Welcome. 🤓

This is my Nix configuration I share across my Macbook Pro and NixOS desktop in my office.

# Features
* [100% flake driven](https://github.com/dustinlyons/nixos-config/blob/main/flake.nix), no use of channels or `configuration.nix`
* `home-manager` module for seamless configuration (no extra clunky CLI steps)
* [Simple nix-command](https://github.com/dustinlyons/nixos-config/tree/main#bootstrap-new-computer) to start from zero, both x86 and MacOS platforms
* Fully declarative [MacOS dock](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) and MacOS [App Store apps](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix)
* Fully managed, auto-updating [homebrew](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) environment _(yes, Nix manages homebrew!)_
* Easily [share](https://github.com/dustinlyons/nixos-config/tree/main/common) config across Linux and Mac with both Nix and Home Manager
* Declarative disk management, say goodbye to disk utils
* Declarative secrets for SSH, PGP, syncthing, and other tools
* Bleeding edge Emacs that fixes itself, thanks to a community [overlay](https://github.com/nix-community/emacs-overlay)
* Extensively configured NixOS environment including clean aesthetic + [window animations](https://github.com/dustinlyons/nixos-config/blob/main/nixos/default.nix)
* Auto-loading of Nix [overlays](https://github.com/dustinlyons/nixos-config/tree/main/overlays): drop a file in a dir and it runs _(great for patches!)_
* No-fuss Syncthing: managed keys, certs, and configuration across all platforms
* Large Emacs [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/config.org) to explore (if that's your thing)
* Optimized for simplicity and readability in all cases, not small files spread across collections of modules

# Videos 
## MacOS
### Updating dependencies with one command
https://github.com/dustinlyons/nixos-config/assets/1292576/2168d482-6eea-4b51-adc1-2ef1291b6598

### Instant Emacs 29 thanks to daemon mode
#### GUI
https://github.com/dustinlyons/nixos-config/assets/1292576/66001066-2bbf-4492-bc9e-60ea1abeb987

#### Terminal
https://github.com/dustinlyons/nixos-config/assets/1292576/d96f59ce-f540-4f14-bc61-6126a74f9f52

## NixOS
https://github.com/dustinlyons/nixos-config/assets/1292576/fa54a87f-5971-41ee-98ce-09be048018b8

### Coming Soon
* ✅ ~Persistence defined under [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) ([#5](https://github.com/dustinlyons/nixos-config/issues/5))~
* ~Secrets managed with `agenix` ([#6](https://github.com/dustinlyons/nixos-config/issues/6))~
* Opt-in persistence using [impermanence](https://github.com/nix-community/impermanence) and `zfs` snapshot reset ([#8](https://github.com/dustinlyons/nixos-config/issues/8))

# Layout

```
.
├── bin          # Optional scripts used to run build/update
├── common       # Shared configurations applicable to all machines
├── hardware     # Hardware-specific configuration
├── darwin       # MacOS and nix-darwin configuration
├── nixos        # My NixOS desktop-related configuration
├── overlays     # Drop an overlay file in this dir, and it runs. So far mainly patches.
└── vms          # VM-specific configs running in my home-lab
```

# Bootstrap New Computer

## For MacOS
### Install dependencies
```sh
xcode-select --install
```
```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```
```sh
nix run nix-darwin -- switch --flake ~/.config/nix-darwin
```
### Install config
```sh
nix --experimental-features 'nix-command flakes' build .#darwinConfigurations.Dustins-MBP.system --impure && \
./result/sw/bin/darwin-rebuild switch --flake .#Dustins-MBP --impure && \
unlink ./result
```

## For NixOS, one command does the trick
Download and burn [the minimal ISO image](https://nixos.org/download.html), then run:

> [!IMPORTANT]
> For Nvidia cards, select the second option `nomodeset` when booting the installer.

> [!NOTE]
> Currently, you need to set the user password on first boot. When you see the login screen, `Ctrl-Alt-F2` gives you another shell where you can login as root and run `passwd <user>`. `Ctrl-Alt-F7` gets back to the login screen.

> [!WARNING]
> Running these commands will reformat your entire drive to the ext4 filesystem.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#secrets
```
```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#install
```

# Live ISO
Not yet available. Coming soon.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#live
```

# Making changes
You can edit your configuration and save the changes.

## Build and switch to new generation
### On Mac
```sh
nix --experimental-features 'nix-command flakes' build .#darwinConfigurations.Dustins-MBP.system --impure && \
./result/sw/bin/darwin-rebuild switch --flake .#Dustins-MBP --impure && \
unlink ./result
```
### On NixOS
```sh
sudo nixos-rebuild switch --flake .#felix
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
