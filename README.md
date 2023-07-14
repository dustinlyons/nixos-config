<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

_Psst: I can help write Nix at your company. <a href="https://twitter.com/dustinhlyons">Get in touch.</a>_
# Overview
Hey, you made it! Welcome. 🤓

You've stumbled upon my personal journey with Nix. For over a year, I've been hacking away on this configuration. It drives my office PC, M1 Macbook, and virtual machines in my home lab. Along with syncthing to manage data, this Nix configuration guarantees I have a working, seamless experience across each machine I use. 

Immutable, reproducible infrastructure rocks! It's game changing and I'll never go back to typing commands in a terminal.

While developing, I've done my best to keep it simple - for both future me and readers like you. You'll see that in how I've organized code, as I keep filename conventions the same across modules. To get you started, I've included step-by-step instructions on bootstrapping a new machine below.

Feel free to open a Github Issue if you run into any problems or have questions. Enjoy Nix!
# Videos 
## NixOS
https://user-images.githubusercontent.com/1292576/229024755-c44f80a8-9257-4d6b-be7c-e37d3a325dd0.mp4

## MacOS
### Updating dependencies with one command

https://github.com/dustinlyons/nixos-config/assets/1292576/30550473-ed22-46a9-b277-e6a56fd67f51

### Instant Emacs 29 thanks to daemon mode
https://github.com/dustinlyons/nixos-config/assets/1292576/66922503-944e-47dd-849d-6baf3e0952a1.mp4

# Features
* Multiple Nix and NixOS configurations, including desktop, laptop, server
* [Step-by-step instructions](https://github.com/dustinlyons/nixos-config/tree/main#bootstrap-new-computer) to start from zero, both x86 and MacOS platforms
* Fully declarative [MacOS dock](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) and MacOS [App Store apps](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix)
* Defined using a [single flake](https://github.com/dustinlyons/nixos-config/blob/main/flake.nix) and two targets, not small files spread across collections of modules
* Fully managed, auto-updating [homebrew](https://github.com/dustinlyons/nixos-config/blob/main/darwin/home-manager.nix) environment
* Easily [share](https://github.com/dustinlyons/nixos-config/tree/main/common) config across Linux and Mac with both Nix and Home Manager
* Minimal [shell scripts](https://github.com/dustinlyons/nixos-config/tree/main/bin) covering basic functions for running systems
* Bleeding edge Emacs that fixes itself, thanks to a community [overlay](https://github.com/nix-community/emacs-overlay)
* Extensively configured NixOS environment including clean aesthetic + [window animations](https://github.com/dustinlyons/nixos-config/blob/main/nixos/default.nix)
* Auto-loading of Nix [overlays](https://github.com/dustinlyons/nixos-config/tree/main/overlays): drop a file in a dir and it runs _(great for patches!)_
* Large Emacs [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/config.org) to explore (if that's your thing)
* Optimized for simplicity and readability in all cases

### Coming Soon
* ✅ ~Persistence defined under [XDG](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html) ([#5](https://github.com/dustinlyons/nixos-config/issues/5))~
* Opt-in persistence using [impermanence](https://github.com/nix-community/impermanence) and `zfs` snapshot reset ([#8](https://github.com/dustinlyons/nixos-config/issues/8))
* Secrets managed with `sops-nix` ([#6](https://github.com/dustinlyons/nixos-config/issues/6))

# Layout

```
.
├── bin          # Simple scripts used to wrap the build
├── common       # Shared configurations applicable to all machines
├── hardware     # Hardware-specific configuration
├── darwin       # MacOS and nix-darwin configuration
├── nixos        # My NixOS desktop-related configuration
├── overlays     # Drop an overlay file in this dir, and it runs. So far mainly patches.
└── vms          # VM-specific configs running in my home-lab
```

# Bootstrap New Computer

## Step 1 - For MacOS, install Nix package manager
Install the nix package manager, add unstable channel:
```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```
```sh
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
```
```sh
nix-channel --update
```

## Step 2 - For NixOS, create a disk partition and install media
Follow this [step-by-step guide](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md) for instructions to install using `ZFS` or `ext3`.


## Step 3 - Install home-manager
Add the home-manager channel and install it:
```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
```
```sh
nix-channel --update
```

## Step 4 - If MacOS, install Darwin dependencies
Install Xcode CLI tools and nix-darwin:
```sh
xcode-select --install
```
```sh
nix-build https://github.com/LnL7/nix-darwin/archive/master.tar.gz -A installer
```
```sh
./result/bin/darwin-installer
```

## Step 5 - Build the environment
Download this repo and run:
```sh
./bin/build
```

## Step 6 - Reboot computer
That's it. You're done.

# Update Computer

## Update dependencies
```sh
nix flake update
```
## Run  build
```sh
./bin/build
```

## Read my occasional musings on Nix

[![Follow @dustinhlyons](https://github.com/dustinlyons/dustinlyons/assets/1292576/3d214b95-6c93-4967-8c72-862fa494e664)](https://www.twitter.com/dustinhlyons)


> "All we have to decide is what to do with the time that is given us." - J.R.R. Tolkien

## Star History

[![Star History Chart](https://api.star-history.com/svg?repos=dustinlyons/nixos-config&type=Date)](https://star-history.com/#dustinlyons/nixos-config&Date)
