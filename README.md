<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config

# Overview

These are "dotfiles" for my

* M1 Macbook Pro, 
* NixOS workstation, 
* and VMs running in my home-lab

Continue reading to configure your own Nix or NixOS install, or use this code as an example.

Some helpful links:
* [My steps to bootstrap a new virtual machine](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md). The same steps apply if you're starting from scratch on bare metal (i.e, no hard disk partition).
* My Emacs [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/Emacs.org)

My NixOS configuration is over a year of continuing to abstract and evolve my daily life, personally and professionally. Nix and the [communities](https://github.com/nix-community/emacs-overlay) around [nixpkg](https://github.com/NixOS/nixpkgs) make it enjoyable!

# Layout

```
.
├── bin          # Simple scripts used to wrap the build
├── common       # Baseline configurations applicable to all machines
├── hardware     # Hardware-specific configuration
├── macos        # MacOS and nix-darwin configuration
├── nixos        # My NixOS desktop-related configuration
├── overlays     # Drop an overlay, and it runs. Mainly patches.
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

## Step 4 - If macOS, install Darwin dependencies
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
./bin/darwin-build
```
or
```sh
./bin/nixos-build
```

## Step 6 - Add Yubikey and generate key
Insert Yubikey and generate private keys
```sh
ssh-keygen -t ecdsa-sk
```

## Step 7 - Reboot computer
That's it. You're done.

# Update Computer

## Download the latest updates and update lock file
```sh
nix flake update
```
## Run platform-specific build
```sh
./bin/darwin-build
```
or
```sh
./bin/nixos-build
```

## You made it this far
Add me on [Twitter](https://twitter.com/dustinhlyons).
