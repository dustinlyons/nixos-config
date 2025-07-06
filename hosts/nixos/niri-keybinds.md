# Niri Keybindings Reference

This document lists all configured keybindings for the Niri Wayland compositor.

## Application Launchers

| Keybinding | Action |
|------------|--------|
| `Ctrl+Alt+T`, `Mod+T` | Launch Alacritty terminal |
| `Mod+Return` | Launch floating terminal |
| `Mod+B` | Launch Google Chrome |
| `Mod+E` | Launch Emacs GUI |
| `Mod+Space` | Launch Fuzzel (application launcher) |

## Window Management

| Keybinding | Action |
|------------|--------|
| `Mod+Q`, `Alt+F4` | Close window |
| `Mod+W`, `Mod+Shift+W` | Toggle window floating |
| `Mod+F` | Maximize column |
| `Mod+Shift+F` | Fullscreen window |
| `Mod+R`, `Mod+Ctrl+R` | Switch preset column width |
| `Mod+Shift+R` | Reset window height |
| `Mod+Shift+E` | Quit Niri |

## Navigation

### Focus Movement
| Keybinding | Action |
|------------|--------|
| `Mod+H`, `Mod+Left`, `Ctrl+Alt+Left` | Focus column left |
| `Mod+L`, `Mod+Right`, `Ctrl+Alt+Right`, `Mod+Tab` | Focus column right |
| `Mod+K` | Focus window up |
| `Mod+J` | Focus window down |
| `Mod+Shift+Tab` | Focus column left |
| `Mod+N` | Focus next window (including floating) |
| `Mod+P` | Focus previous window (including floating) |
| `Mod+Ctrl+H` | Focus first column |
| `Mod+Ctrl+L` | Focus last column |

### Window/Column Movement
| Keybinding | Action |
|------------|--------|
| `Mod+Shift+H` | Move column left |
| `Mod+Shift+L` | Move column right |
| `Mod+Shift+K` | Move window up |
| `Mod+Shift+J` | Move window down |
| `Mod+Comma` | Consume window into column |
| `Mod+Period` | Expel window from column |
| `Mod+Shift+C` | Center column |

### Workspace Navigation
| Keybinding | Action |
|------------|--------|
| `Mod+1` to `Mod+6` | Focus workspace 1-6 |
| `Mod+Up`, `Ctrl+Alt+Up` | Focus workspace up |
| `Mod+Down`, `Ctrl+Alt+Down` | Focus workspace down |
| `Mod+Shift+1` to `Mod+Shift+6` | Move column to workspace 1-6 |
| `Mod+Shift+Up` | Move column to workspace up |
| `Mod+Shift+Down` | Move column to workspace down |

## Window Sizing

| Keybinding | Action |
|------------|--------|
| `Mod+Minus` | Decrease column width by 10% |
| `Mod+Equal` | Increase column width by 10% |
| `Mod+Shift+Minus` | Decrease window height by 10% |
| `Mod+Shift+Equal` | Increase window height by 10% |

## Multi-Monitor Support

| Keybinding | Action |
|------------|--------|
| `Mod+Shift+Left` | Focus left monitor |
| `Mod+Shift+Right` | Focus right monitor |
| `Mod+Ctrl+Shift+Left` | Move column to left monitor |
| `Mod+Ctrl+Shift+Right` | Move column to right monitor |

## Special Functions

| Keybinding | Action |
|------------|--------|
| `Mod+V` | Clipboard history (via Fuzzel) |
| `Mod+C` | Color picker |
| `Print` | Screenshot selection |
| `Ctrl+Print` | Screenshot full screen |
| `Alt+Print` | Screenshot window |
| `Ctrl+Alt+L`, `Mod+Ctrl+Q` | Lock screen |
| `Mod+Shift+Slash` | Show keybindings help overlay |

## Media Controls

| Keybinding | Action |
|------------|--------|
| `XF86AudioRaiseVolume` | Volume up 5% |
| `XF86AudioLowerVolume` | Volume down 5% |
| `XF86AudioMute` | Toggle mute |
| `Mod+Shift+P` | Play/pause media |
| `Mod+Shift+N` | Next track |
| `Mod+Shift+B` | Previous track |

## Terminal Animations

| Keybinding | Action |
|------------|--------|
| `Mod+F1` | ASCII aquarium (fullscreen) |
| `Mod+F2` | Terminal clock (fullscreen) |
| `Mod+F3` | Audio visualizer - Cava (fullscreen) |

## Notes

- `Mod` key is typically the Super/Windows key
- Preset column widths cycle through: 1/3, 1/2, and 2/3 of screen width
- Floating terminals open with `app-id="floating-terminal"`
- Terminal animations open fullscreen on the next workspace