<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config

# Overview
Hey there! Welcome to my Nix and NixOS configuration. Here you'll find my "dotfiles" that I use on my personal machines and servers, like my M1 Macbook Pro, my NixOS workstation, and even the VMs running in my home-lab.

Whether you're looking to set up your own Nix or NixOS installation or just want to take a look at my code for inspiration, you're in the right place. I've spent a lot of time making my NixOS config as simple and user-friendly as possible, and it's been a blast. Nix and the [communities](https://github.com/nix-community/emacs-overlay) around [nixpkg](https://github.com/NixOS/nixpkgs) have made it all possible.

To get you started, I've included my [steps for bootstrapping a new virtual machine](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md) that also work for a bare metal setup. For MacOS instructions, just check out the details further below. And if you're new to Emacs, take a look at my [literate config](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/Emacs.org). I love hacking Emacs and you'll see it's much more than an IDE.

I've been using tools like Nix and Emacs to make my daily life both personally and professionally better, and I hope you'll find something here that will help you too. If you have any questions or run into any issues, feel free to open a Github issue. I'm always happy to help.

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
./bin/build
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
## Run  build
```sh
./bin/build
```

## You made it this far
Add me on [Twitter](https://twitter.com/dustinhlyons).
