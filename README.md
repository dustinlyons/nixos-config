# Dustin's Nix / NixOS config

## Overview
#### Updated: October 27, 2021

These are my "dotfiles" contained within Nix deriviations that drive setup and configuration of my Macbook Pro and home-lab desktop machines.

In a previous life my configuration was driven by [Guix](https://github.com/dustinlyons/guix-config), but with the recent release of Apple Silicon I've decided to seek out better Darwin support. 

Enter Nix and NixOS.

## Features

- Simplicity as a guiding principle; Nix is notoriously confusing so make it easy to understand
- Entirely driven by my single Flake
- Supports sharing config between an M1 Macbook Pro and NixOS PCs
- Simple bash scripts to build and switch environments
- Literate configuration style, so overly explain for posterity

### Get in touch
- Feedback or questions? Find me on [Twitter](https://twitter.com/dustinhlyons).
