# Dustin's Nix / NixOS config

## Overview

These are my "dotfiles" contained within Nix derivations that drive the setup and configuration of

* my Macbook Pro
* a NixOS workstation sitting on my desk at home
* an old Thinkpad I use as an air-gapped machine
* virtual machines running on my home-lab Proxmox server

Why do this? Why spend the hours to learn Nix and the nuances of getting it all to work across various architectures?

Well, it's pretty magical, honestly.

Years of honing my environment, making tiny optimizations to my workflow and development environment, are now codified and reproducible by a machine.

I'll never experience a day, _sans hardware failure_, where everything is borked and I can't work, thanks to the declarative nature of Nix, the Nix Store, and its update and rollback features.

I can magically update everything, everywhere, by typing `nix flake update; nixos-rebuild switch`. 

Done. 

It's great.

I do this frequently, _because it's so damn cheap_, but mainly I get to leverage the work from _hundreds_ of people, all contributing pull requests in harmony to maintain a global set of software that's highly secure, efficient, and workable. 

Sometimes I go to update, and the build fails. That's okay. Nix doesn't apply the update to your machine until everything works.

So usually, I just wait a few days for the community to patch it and move on. I can count on one finger the times I've had an issue, and there wasn't already an active discussion.

Seriously, the [nixpkgs](https://github.com/NixOS/nixpkgs) repository and other groups within [nix-community](https://github.com/nix-community/), like [emacs-overlay](https://github.com/nix-community/emacs-overlay), are some of the best examples of open source at scale. PRs are merged multiple times an hour (over 150,000 😱 closed), and you get all of that with a simple `nix flake update`. 

It's brilliant. And it's not just installing _updates_. `emacs-overlay`, for example, helps me keep a bleeding edge, natively compiled version of Emacs, across both M1 and x86, with minimal effort. Any workarounds or known ~platform~ M1 bugs are quickly solved for me.

So yeah, pretty great. I encourage you to give Nix a try. Look around, and if you have questions, I'm on [Twitter](https://twitter.com/dustinhlyons).

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
Insert laptop Yubikey and generate private keys
```sh
ssh-keygen -t ecdsa-sk
```

### Step 6 - Reboot computer
That's it. You're done.
