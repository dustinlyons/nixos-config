<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

_Psst: I can help write Nix at your company. <a href="https://twitter.com/dustinhlyons">Get in touch.</a>_
# Overview
Hey, you made it! Welcome. 🤓

You've stumbled upon my personal journey with Nix. For over a year, I've been hacking away at this configuration. This repository drives my office PC, M1 Macbook, and virtual machines in my home lab. Along with syncthing to manage data, this Nix configuration guarantees I have a working, seamless experience across each machine I use.

While developing, I've done my best to keep it simple - for both future me and readers like you. You'll see that in how I've organized code, as I keep filename conventions the same across modules. To get started, you can bootstrap from bare metal, run on your Mac, or try out a live ISO. See below.

# Videos 
## NixOS
https://github.com/dustinlyons/nixos-config/assets/1292576/fa54a87f-5971-41ee-98ce-09be048018b8

## MacOS
### Updating dependencies with one command
https://github.com/dustinlyons/nixos-config/assets/1292576/2168d482-6eea-4b51-adc1-2ef1291b6598

### Instant Emacs 29 thanks to daemon mode
#### GUI
https://github.com/dustinlyons/nixos-config/assets/1292576/66001066-2bbf-4492-bc9e-60ea1abeb987

#### Terminal
https://github.com/dustinlyons/nixos-config/assets/1292576/d96f59ce-f540-4f14-bc61-6126a74f9f52

# Features
* Multiple Nix and NixOS configurations across Mac and Linux, including desktop, laptop, server
* [A single nix-command](https://github.com/dustinlyons/nixos-config/tree/main#bootstrap-new-computer) to start from zero, both x86 and MacOS platforms
* Fully declarative [MacOS dock](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) and MacOS [App Store apps](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix)
* [100% flake driven](https://github.com/dustinlyons/nixos-config/blob/main/flake.nix), no use of channels or `configuration.nix` during install or after
* Fully managed, auto-updating [homebrew](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) environment _(yes, Nix manages homebrew!)_
* Easily [share](https://github.com/dustinlyons/nixos-config/tree/main/common) config across Linux and Mac with both Nix and Home Manager
* Bleeding edge Emacs that fixes itself, thanks to a community [overlay](https://github.com/nix-community/emacs-overlay)
* Extensively configured NixOS environment including clean aesthetic + [window animations](https://github.com/dustinlyons/nixos-config/blob/main/nixos/default.nix)
* Auto-loading of Nix [overlays](https://github.com/dustinlyons/nixos-config/tree/main/overlays): drop a file in a dir and it runs _(great for patches!)_
* Large Emacs [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/config.org) to explore (if that's your thing)
* Optimized for simplicity and readability in all cases, not small files spread across collections of modules

### Coming Soon
* ✅ ~Persistence defined under [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) ([#5](https://github.com/dustinlyons/nixos-config/issues/5))~
* Opt-in persistence using [impermanence](https://github.com/nix-community/impermanence) and `zfs` snapshot reset ([#8](https://github.com/dustinlyons/nixos-config/issues/8))
* Secrets managed with `sops-nix` ([#6](https://github.com/dustinlyons/nixos-config/issues/6))

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
> Until I ship `sops-nix`, you'll need to set the user password on first boot. `Ctrl-Alt-F2` gives you another shell where you can login as root and run `passwd <user>`. `Ctrl-Alt-F7` gets back to the login screen.

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
