<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Dustin's Nix / NixOS config
![GitHub last commit](https://img.shields.io/github/last-commit/dustinlyons/nixos-config?style=plastic)

# Overview
> "All we have to decide is what to do with the time that is given us." - J.R.R. Tolkien, The Fellowship of the Ring

Well hello there! I promise you're in the right place. You've stumbled upon the results of my multi-year long journey learning Nix, building my personal machines and servers, like my M1 Macbook Pro, NixOS workstation, and the VMs running in my home-lab. Further below, I've got instructions for setting up your own Nix or NixOS installation, or feel free to just poke around and take the bits you like. 

I started with Nix because was I enticed by a functional operating system and the great synergy between MacOS and x86 PCs. This was important; I still use a clunky linux box in my office when I'm at home, and a Macbook for when I'm out. I've tried other laptops on the market, including the linux-friendly ones like System76, but everything is still subpar to the Macbook. Quality is unrivaled.

So Nix, with this cool property of being both a package manager and an OS, was perfect. And to this today, that still holds true. You'll see I share almost 80% of config between the machines I use, and syncthing is used to manage state. The result is a seamless, single environment across my two machines that just works. 

To get you started, I've included my [steps for bootstrapping a new virtual machine](https://github.com/dustinlyons/nixos-config/blob/main/vm/README.md) that also work for a bare metal setup. For MacOS instructions, you'll need to install a few dependencies I've listed below. And if you're new to Emacs, take a look at my [literate config](https://github.com/dustinlyons/nixos-config/blob/main/common/config/emacs/Emacs.org). I love hacking Emacs and you'll see it's much more than just an IDE. 🤓

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

Thank you Nix and the [communities](https://github.com/nix-community/emacs-overlay) around [nixpkg](https://github.com/NixOS/nixpkgs).
