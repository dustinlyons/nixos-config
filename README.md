<img src="https://user-images.githubusercontent.com/1292576/190241835-41469235-f65d-4d4b-9760-372cdff7a70f.png" width="48">

# Nix for MacOS + NixOS
[![Build Starter Template](https://github.com/dustinlyons/nixos-config/actions/workflows/build.yml/badge.svg)](https://github.com/dustinlyons/nixos-config/actions/workflows/build.yml)

# Overview
Hey, you made it! Welcome. 🤓 

Nix is a powerful package manager for Linux and Unix systems that ensures reproducible, declarative, and reliable software management. 

This repository contains configuration for a general-purpose development environment that runs on MacOS, NixOS, or both simultaneously. 

I use it daily on my 🧑🏻‍💻 M1 Macbook Pro and an x86 PC in my home office. It also runs as a VM on your Mac. Others have reported that it's working for them too.

Check out the starter templates and step-by-step commands below to get started!

## Table of Contents
- [Nix for MacOS + NixOS](#nix-for-macos--nixos)
  - [Overview](#overview)
    - [Table of Contents](#table-of-contents)
    - [Layout](#layout)
    - [Features](#features)
  - [Videos](#videos)
    - [MacOS](#macos)
      - [Updating dependencies with one command](#updating-dependencies-with-one-command)
      - [Instant Emacs 29 thanks to daemon mode](#instant-emacs-29-thanks-to-daemon-mode)
    - [NixOS](#nixos)
  - [Installing](#installing)
    - [For MacOS](#for-macos)
      - [1. Install dependencies](#1-install-dependencies)
      - [2. Install Nix](#2-install-nix)
      - [3. Initialize a starter template](#3-initialize-a-starter-template)
      - [4. Apply your current user info](#4-apply-your-current-user-info)
      - [5. Decide what packages to install](#5-decide-what-packages-to-install)
      - [6. Optional: Setup secrets](#6-optional-setup-secrets)
      - [7. Install configuration](#7-install-configuration)
    - [For NixOS](#for-nixos)
      - [1. Burn the latest ISO](#1-burn-the-latest-iso)
      - [2. Optional: Setup secrets](#2-optional-setup-secrets)
      - [3. Install configuration](#3-install-configuration)
      - [4. Set user password](#4-set-user-password)
  - [How to Create Secrets](#how-to-create-secrets)
  - [Live ISO](#live-iso)
  - [Deploying Changes to Your System](#deploying-changes-to-your-system)
    - [For MacOS](#for-macos-1)
    - [For NixOS](#for-nixos-1)
    - [Update Dependencies](#update-dependencies)
  - [Compatibility and Testing](#compatibility-and-testing)
  - [Contributing](#contributing)
  - [Feedback and Questions](#feedback-and-questions)
  - [License](#license)
  - [Appendix](#appendix)
    - [Why Nix Flakes](#why-nix-flakes)
    - [NixOS Components](#nixos-components)
    - [Stars](#stars)
    - [Support](#support)


## Layout
```
.
├── bin          # Optional scripts used to run build/update
├── hosts        # Host-specific configuration
├── modules      # MacOS and nix-darwin, NixOS, and shared configuration
├── overlays     # Drop an overlay file in this dir, and it runs. So far, mainly patches.
├── templates    # Starter versions of this configuration
```

## Features
- **Nix Flakes**: 100% flake driven, no `configuration.nix`, [no Nix channels](#why-nix-flakes)─ just `flake.nix`
- **Same Environment Everywhere**: Easily share config across Linux and Mac (both Nix and Home Manager)
- **MacOS Dream Setup**: Fully declarative MacOS, including UI, dock and MacOS App Store apps
- **Simple Bootstrap**: Simple Nix commands to start from zero, both x86 and MacOS platforms
- **Managed Homebrew**: Zero maintenance homebrew environment with `nix-darwin` and `nix-homebrew`
- **Disk Management**: Declarative disk management with `disko`, say goodbye to disk utils
- **Secrets Management**: Declarative secrets with `agenix` for SSH, PGP, syncthing, and other tools
- **Super Fast Emacs**: Bleeding edge Emacs that fixes itself, thanks to a community overlay
- **Built In Home Manager**: `home-manager` module for seamless configuration (no extra clunky CLI steps)
- **NixOS Environment**: Extensively configured NixOS including clean aesthetic + window animations
- **Nix Overlays**: [Auto-loading of Nix overlays](https://github.com/dustinlyons/nixos-config/tree/main/overlays): drop a file in a dir and it runs (great for patches!)
- **Declarative Sync**: No-fuss Syncthing: managed keys, certs, and configuration across all platforms
- **Emacs Literate Configuration**: [Large Emacs literate configuration](https://github.com/dustinlyons/nixos-config/blob/main/shared/config/emacs/config.org) to explore (if that's your thing)
- **Simplicity and Readability**: Optimized for simplicity and readability in all cases, not small files everywhere

## Videos
### MacOS
#### Updating dependencies with one command
https://github.com/dustinlyons/nixos-config/assets/1292576/2168d482-6eea-4b51-adc1-2ef1291b6598

#### Instant Emacs 29 thanks to daemon mode
- **GUI**

https://github.com/dustinlyons/nixos-config/assets/1292576/66001066-2bbf-4492-bc9e-60ea1abeb987

- **Terminal**

https://github.com/dustinlyons/nixos-config/assets/1292576/d96f59ce-f540-4f14-bc61-6126a74f9f52

### NixOS

https://github.com/dustinlyons/nixos-config/assets/1292576/fa54a87f-5971-41ee-98ce-09be048018b8

# Installing
> [!IMPORTANT]
> Note: Nix 2.18 currently [has a bug](https://github.com/NixOS/nix/issues/9052) that impacts this repository.

For now, if you run into errors like this:
```
error: path '/nix/store/52k8rqihijagzc2lkv17f4lw9kmh4ki6-gnugrep-3.11-info' is not valid
```

Run `nix copy` to make the path valid.
```
nix copy --from https://cache.nixos.org /nix/store/52k8rqihijagzc2lkv17f4lw9kmh4ki6-gnugrep-3.11-info
```

## For MacOS
I've tested these instructions on a fresh Macbook Pro as of September 2023.

### 1. Install dependencies
```sh
xcode-select --install
```

### 2. Install Nix
Thank you for the installer, [Determinate Systems](https://determinate.systems/)!
```sh
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

### 3. Initialize a starter template
This is a simplified version without secrets management.
```sh
mkdir -p nixos-config && (cd nixos-config && nix flake --extra-experimental-features 'nix-command flakes' init -t github:dustinlyons/nixos-config#starter)
```

This is a full version with secrets management.
```sh
mkdir -p nixos-config && (cd nixos-config && nix flake --extra-experimental-features 'nix-command flakes' init -t github:dustinlyons/nixos-config#starterWithSecrets)
```

### 4. Apply your current user info
Run this Nix app to replace stub values with your username, full name, and email.
```sh
nix run .#apply
```

### 5. Decide what packages to install
You can search for packages on the [official NixOS website](https://search.nixos.org/packages).

**Review these files**

* `darwin/casks`
* `darwin/packages`
* `darwin/home-manager`
* `nixos/packages`
* `shared/packages`

### 6. Optional: Setup secrets
If you are using the starter with secrets, there are a few additional steps.

#### 6a. Create a private Github repo to hold your secrets
In Github, create a private [`nix-secrets`](https://github.com/dustinlyons/nix-secrets-example) repository. You'll enter this name during installation.

#### 6b. Install keys
Before generating your first build, these keys must exist in your `~/.ssh` directory. Don't worry, we provide a few commands to help you.

| Key Name            | Platform         | Description                           | 
|---------------------|------------------|---------------------------------------|
| id_ed25519          | MacOS / NixOS    | Used to download secrets from Github. |
| id_ed25519_agenix   | MacOS / NixOS    | Used to encrypt and decrypt secrets.  |

You must one run of these commands:

##### Copy keys from USB drive
This command auto-detects a USB drive connected to the current system.
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
nix run .#copyKeys
```

##### Create new keys
```sh
nix run .#createKeys
```

##### Check existing keys
If you're rolling your own, just check they are installed correctly.
```sh
nix run .#checkKeys
```

### 7. Install configuration
First-time installations require you to move the current `/etc/nix/nix.conf` out of the way.
```sh
[ -f /etc/nix/nix.conf ] && sudo mv /etc/nix/nix.conf /etc/nix/nix.conf.before-nix-darwin
```

If you're using a git repository, only files in the working tree will be copied to the [Nix Store](https://zero-to-nix.com/concepts/nix-store). 

So it's imperative you run `git add .`.

Then, you can build the system closure but not switch to the new generation by doing a dry run:
```sh
nix run .#dryRun
```

Finally, to both build and install the configuration, run:
```sh
nix run .#build
```

From here on, if you want to make changes, edit your configuration and always run:
```sh
nix run .#build
```

## For NixOS
This configuration supports both `x86_64` and `aarch64` platforms.

### 1. Burn the latest ISO
Download and burn [the minimal ISO image](https://nixos.org/download.html) to a USB, or create a new VM with the ISO as base.
> Note, if you're building a VM on an Apple Silicon Mac, choose [64-bit ARM](https://channels.nixos.org/nixos-23.05/latest-nixos-minimal-aarch64-linux.iso).

**Quick Links**

* [64-bit Intel/AMD](https://channels.nixos.org/nixos-23.05/latest-nixos-minimal-x86_64-linux.iso)
* [64-bit ARM](https://channels.nixos.org/nixos-23.05/latest-nixos-minimal-aarch64-linux.iso)

Boot the installer.
### 2. Optional: Setup secrets
If you are using the starter with secrets, there are a few additional steps.

#### 2a. Create a private Github repo to hold your secrets
In Github, create a private [`nix-secrets`](https://github.com/dustinlyons/nix-secrets-example) repository. You'll enter this name during installation.

#### 2b. Install keys
Before generating your first build, these keys must exist in your `~/.ssh` directory. Don't worry, we provide a few commands to help you.

| Key Name            | Platform         | Description                           | 
|---------------------|------------------|---------------------------------------|
| id_ed25519          | MacOS / NixOS    | Used to download secrets from Github. |
| id_ed25519_agenix   | MacOS / NixOS    | Used to encrypt and decrypt secrets.  |

You must one run of these commands:

##### Copy keys from USB drive
This command auto-detects a USB drive connected to the current system.
> Keys must be named `id_ed25519` and `id_ed25519_agenix`.
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#copyKeys
```

##### Create new keys
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#createKeys
```

##### Check existing keys
If you're rolling your own, just check they are installed correctly.
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#checkKeys
```

### 3. Install configuration
#### Run command
After the keys are in place, you're good to go. Run either of these commands:

> [!IMPORTANT]
> For Nvidia cards, select the second option, `nomodeset`, when booting the installer, or you will see a blank screen.

> [!WARNING]
> Running this will reformat your drive to the ext4 filesystem.

**Simple**
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#install
```

**With secrets**
```sh
sudo nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#installWithSecrets
```

### 4. Set user password
On first boot at the login screen:
- Use the shortcut `Ctrl-Alt-F2` (or `Fn-Ctrl-Option-F2` if on a Mac) to move to a terminal session
- Login as `root` using the password created during installation
- Set the user password with `passwd <user>`
- Go back to the login screen: `Ctrl-Alt-F7`

# How to create secrets
To create a new secret `secret.age`, first [create a `secrets.nix` file](https://github.com/ryantm/agenix#tutorial) at the root of your `nix-secrets` repository. This will only be used by the `agenix` CLI command, but must exist before we get started.

**secrets.nix**
```nix
let
  user1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0idNvgGiucWgup/mP78zyC23uFjYq0evcWdjGQUaBH";
  users = [ user1 ];

  system1 = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPJDyIr/FSz1cJdcoW69R+NrWzwGK/+3gJpqD1t8L2zE";
  systems = [ system1 ];
in
{
  "secret.age".publicKeys = [ user1 system1 ];
}
```
Now that we've configured `agenix` with our `secrets.nix`, it's time to create our first secret. Run the command below. Note, it assumes your SSH private key is in `~/.ssh/` or you can provide the `-i` flag with a path to your `id_ed25519` key. 
```
EDITOR=vim nix run github:ryantm/agenix -- -e secret.age
```
This opens an editor to accept, encrypt, and write your secret to disk. 

Commit the file to your `nix-secrets` repo and add a reference in the `secrets.nix` of your `nix-secrets` repository. References look like
```
{
  "secret.age".publicKeys = [ user1 system1 ];
}
```
where `"secret.age"` is your new filename. Now we have two files: `secrets.nix` and our `secret.age`. Let me show you.

## Example
Let's say I wanted to create a new secret to hold my Github SSH key. 

I would `cd` into my `nix-secrets` repo directory, verify the `agenix` configuration (named `secrets.nix`) exists, then run 
```
EDITOR=vim nix run github:ryantm/agenix -- -e github-ssh-key.age
```

This would start a `vim` session.

I would enter insert mode `:i`, copy+paste the key, hit Esc and then type `:w` to save it, resulting in the creation of a new file, `github-ssh-key.age`.

Next, I would edit `secrets.nix` to include a line specifying the public key to use for my new secret. I specify a user key, but I could just as easily specify a host key.

**secrets.nix**
```nix
let
  dustin = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL0idNvgGiucWgup/mP78zyC23uFjYq0evcWdjGQUaBH";
  users = [ dustin ];
  systems = [ ];
in
{
  "github-ssh-key.age".publicKeys = [ dustin ];
}
```

Then I'd commit all changes to the `nix-secrets` repository, go back to my `nixos-config` and run `nix flake update` to update the lock file.

Now, all that's left is [using the secret](https://github.com/dustinlyons/nixos-config/blob/3b95252bc6facd7f61c6c68ceb1935481cb6b457/nixos/secrets.nix#L28) in my configuration.

# Live ISO
Not yet available. Coming soon.

```sh
nix run --extra-experimental-features 'nix-command flakes' github:dustinlyons/nixos-config#live
```

# Deploying changes to your system
With Nix, changes to your system are made by 
- editing your system configuration
- building the [system closure](https://zero-to-nix.com/concepts/closures)
- creating and switching to it _(i.e creating a [new generation](https://nixos.wiki/wiki/Terms_and_Definitions_in_Nix_Project#generation))_

## For MacOS
```sh
nix run .#build
```
## For NixOS
```sh
nix run .#build
```

## Update dependencies
```sh
nix flake update
```

## Compatibility and Testing
This configuration has been tested and confirmed working on the following platforms:
- M1 Apple Silicon
- Bare metal x86_64 PC
- NixOS inside VMWare on MacOS
- MacOS Sonoma inside Parallels on MacOS

## Contributing
Interested in contributing to this project? Here's how you can help:

- **Code Contributions**: If you're interested in contributing code, please start by looking at open issues or feature requests. Fork the repository, make your changes, and submit a pull request. Make sure your code adheres to the existing style. For significant changes, consider opening an issue for discussion before starting work.

- **Reporting Bugs**: If you encounter bugs or issues, please help by reporting them. Open a GitHub Issue and include as much detail as possible: what you were doing when the bug occurred, steps to reproduce the issue, and any relevant logs or error messages. This information will be invaluable in diagnosing and fixing the problem.

## Feedback and Questions
Have feedback or questions? Feel free to use the [discussion forum](https://github.com/dustinlyons/nixos-config/discussions).

## License
This project is released under the [MIT License](link-to-license).

## Appendix
### Why Nix Flakes
**Reasons to jump into flakes and skip `nix-env`, Nix channels, etc**
- Flakes work just like other package managers you already know: `npm`, `cargo`, `poetry`, `composer`, etc. Channels work more like traditional Linux distributions (like Ubuntu), which most devs don't know.
- Flakes encapsulate not just project dependencies, but Nix expressions, Nix apps, and other configurations in a single file. It's all there in a single file. This is nice.
- Channels lock all packages to one big global `nixpkgs` version. Flakes lock each individual package to a version, which is more precise and makes it much easier to manage overall.
- Flakes have a growing ecosystem (see [Flake Hub](https://flakehub.com/) or [Dev Env](https://devenv.sh/)), so you're future-proofing yourself.
  
### NixOS Components

| Component                   | Description                                     | 
| --------------------------- | :---------------------------------------------  |
| **Window Manager**          | Xorg + bspwm                                    |
| **Terminal Emulator**       | alacritty                                       |
| **Bar**                     | polybar                                         |
| **Application Launcher**    | rofi                                            |
| **Notification Daemon**     | dunst                                           |
| **Display Manager**         | lightdm                                         |
| **File Manager**            | thunar                                          |
| **Text Editor**             | emacs daemon mode                               |
| **Media Player**            | cider                                           |
| **Image Viewer**            | feh                                             |
| **Screenshot Software**     | flameshot                                       |

### Stars

> "All we have to decide is what to do with the time that is given us." - J.R.R. Tolkien

[![Star History Chart](https://api.star-history.com/svg?repos=dustinlyons/nixos-config&type=Date)](https://star-history.com/#dustinlyons/nixos-config&Date)

### Support
[Buy me a coffee](https://www.buymeacoffee.com/dustinlyons1) or [follow me on Twitter](https://twitter.com/dustinhlyons).
