## Common
This gets imported by both MacOS and NixOS modules. Some examples of configuration include git, zsh, vim.

## Layout
```
.
├── config             # Config files not written in Nix
├── snippets           # Git submodules of Emacs snippets
├── default.nix        # Defines how we import overlays 
├── home-manager.nix   # The goods; most all shared config lives here
├── packages.nix       # List of packages to share

```
