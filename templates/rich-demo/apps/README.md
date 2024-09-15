# Apps
The [apps](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-run#apps) in this directory are Nix [installables](https://nix.dev/manual/nix/2.22/command-ref/new-cli/nix?search=#installables), created using the [`mkApp`](https://github.com/dustinlyons/nixos-config/blob/main/flake.nix#L49) function declared within my `flake.nix` file. 

These Nix commands are tailored for different systems, including Linux (`x86_64-linux`, `aarch64-linux`) and Darwin (`aarch64-darwin`, `x86_64-darwin`). 

They execute with `nix run` and are referenced as part of the step-by-step instructions found in the [README](https://github.com/dustinlyons/nixos-config/blob/main/README.md).
