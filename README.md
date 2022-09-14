<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config

## Overview

These are [my](https://twitter.com/dustinhlyons) "dotfiles" written as Nix derivations. My setup involves a Macbook Pro, a NixOS workstation, and a home-lab server running Nix virtual machines. The home-lab server helps me [run my own Dropbox](https://github.com/dustinlyons/nixos-config/blob/main/vm/syncthing/configuration.nix), host my own CI infrastructure, keep spam out of my house, automate stuff, etc. etc. Anyway, it's all here.

Some helpful links:
* [My steps to bootrap a new virtual machine](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md). The same steps apply if you're starting from scratch on bare metal (i.e no hard disk partition).
* My Emacs [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/Emacs.org)

This is over a year's work of continuing to abstract and evolve my day-to-day life, both personally and professionally. Nix and the [communities](https://github.com/nix-community/emacs-overlay) around [nixpkg](https://github.com/NixOS/nixpkgs) really make it enjoyable!

## Update Computer

### Download latest updates and update lock file
```sh
nix flake update
```
### Run platform specific build
```sh
./bin/darwin-build
```
or
```sh
./bin/nixos-build
```

## Bootstrap New Computer

### Step 1 - For foreign distros (namely macOS), install Nix package manager
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

### Step 2 - Install home-manager
Add the home-manager channel and install it:
```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
```
```sh
nix-channel --update
```

### Step 3 - If macOS, install Darwin dependencies
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

### Step 4 - Build the environment
Download this repo and run:
```sh
./bin/darwin-build
```
or
```sh
./bin/nixos-build
```

### Step 5 - Add Yubikey and generate key
Insert Yubikey and generate private keys
```sh
ssh-keygen -t ecdsa-sk
```

### Step 6 - Reboot computer
That's it. You're done.
