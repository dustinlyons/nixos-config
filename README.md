<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config

# Overview
Welcome to my Nix and NixOS configuration! Here, you'll find "dotfiles" I share across various machines. This includes

* M1 Macbook Pro
* NixOS workstation
* VMs running in my home-lab

Whether you're looking to configure your own Nix or NixOS installation or simply want to use this code as an example, I hope you'll find this repository a valuable resource.

I've put in a lot of time and effort to make my NixOS configuration as efficient and user-friendly as possible. It's been a year-long journey of continuing to abstract and evolve my daily life, both personally and professionally, and I've had a blast doing it! Nix and the [communities](https://github.com/nix-community/emacs-overlay) around [nixpkg](https://github.com/NixOS/nixpkgs) have made it all possible.

To help you get started, you'll find my [steps for bootstrapping a new virtual machine](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md), which can also be applied to a bare metal (i.e, no hard disk partition) setup. For MacOS instructions, see details further below. Additionally, if you don't know much about Emacs, take a look at my [literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/Emacs.org). You'll get a sense of what Emacs can do (spoiler: it's not just an IDE.)

Tools like Nix and Emacs make it all possible for me.

If you have any questions about the project or run into any issues, please don't hesitate to open a Github Issue. I'm always happy to help and offer my support. So, let's dive in and start configuring!

# Layout

```
.
├── bin          # Simple scripts used to wrap the build
├── common       # Shared configurations applicable to all machines
├── hardware     # Hardware-specific configuration
├── macos        # MacOS and nix-darwin configuration
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
