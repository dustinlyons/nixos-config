# Dustin's Nix / NixOS config

## Overview
#### Updated: November 18, 2021

These are my "dotfiles" contained within Nix deriviations that drive setup and configuration of my Macbook Pro and a NixOS workstation sittong on my desk at home.

In a previous life my configuration was driven by [Guix](https://github.com/dustinlyons/guix-config), but with the recent release of new Apple Silicon I started exploring other options.

Enter Nix and NixOS.

## Features

- Simplicity as a guiding principle; Nix is notoriously confusing so make it easy to understand
- Entirely driven by my single Flake
- Supports sharing config between an M1 Macbook Pro and NixOS PCs
- Simple bash scripts to build and switch environments
- Literate configuration style to overly explain for posterity (coming soon)

## Install

### Step 1 - For foreign distros (namely MacOS), install Nix package manager
Install the nix package manager:
```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```
```sh
nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
```
```sh
nix-channel --update
```

### Step 2 - Install home-manager (declaratively manage our dotfiles)
Install the nix package manager:
```sh
nix-channel --add https://github.com/nix-community/home-manager/archive/master.tar.gz home-manager
```
```sh
nix-channel --update
```

### Step 3 - If MacOS, install Darwin dependencies
Install nix-darwin, a native set of Nix modules for MacOS, and Xcode CLI tools.
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
Grab the latest nix-config zip file from Github, move to the directory and run
```sh
./bin/mac-build
```
or
```sh
./bin/nixos-build
```

### Step 5 - Add Yubikey and generate key
Insert laptop Yubikey and generate private keys
```sh
ssh-keygen -t ecdsa-sk
```

### Step 6 - Reboot computer
That's it. You're done.

## Get in touch
- Feedback or questions? Find me on [Twitter](https://twitter.com/dustinhlyons).
