{ pkgs }:

let
  # Define the base Emacs package to use
  emacsPackage = if pkgs.stdenv.isDarwin
    then pkgs.emacs
    else pkgs.emacs-unstable-pgtk;

  # Common Emacs packages for both Darwin and NixOS
  emacsWithPackages = (pkgs.emacsPackagesFor emacsPackage).emacsWithPackages (epkgs: with epkgs; [
    # Core functionality
    gcmh
    general
    evil
    evil-collection
    evil-org
    evil-commentary
    undo-tree
    
    # UI and themes
    doom-themes
    doom-modeline
    all-the-icons
    all-the-icons-ivy
    all-the-icons-dired
    dashboard
    rainbow-delimiters
    ace-window
    which-key
    helpful
    
    # Ivy/Counsel framework
    ivy
    counsel
    ivy-rich
    ivy-prescient
    prescient
    counsel-projectile
    swiper
    
    # File management
    treemacs
    treemacs-evil
    treemacs-projectile
    treemacs-all-the-icons
    treemacs-icons-dired
    treemacs-magit
    dired-ranger
    dired-collapse
    
    # Project management
    projectile
    ripgrep
    
    # Deadgrep - dedicated ripgrep UI with full buffer interface
    deadgrep
    
    # Org mode
    org-super-agenda
    org-superstar
    org-transclusion
    org-download
    org-roam
    emacsql
    sqlite3
    visual-fill-column
    
    # Writing
    writeroom-mode
    flyspell-correct
    flyspell-correct-ivy
    yasnippet
    
    # Version control
    magit
    
    # Programming - Language servers
    lsp-mode
    lsp-ui
    lsp-treemacs
    company
    company-box
    
    # Code navigation
    dumb-jump
    
    # Programming - Languages
    go-mode
    nix-mode
    yaml-mode
    markdown-mode
    web-mode
    typescript-mode
    tree-sitter
    tree-sitter-langs
    tide
    prettier-js
    emmet-mode
    rainbow-mode
    
    # Python
    lsp-pyright
    blacken
    
    # PHP
    php-mode
    php-cs-fixer
    
    # Infrastructure
    dockerfile-mode
    terraform-mode
    
    # Other tools
    f  # File manipulation library
    rotate
    exec-path-from-shell
    key-chord  # For double-tap key bindings
    transient  # Required for claude-code.el (0.7.5+)
    eat  # Terminal emulator for claude-code.el
    
    # Note: copilot needs special handling via manual installation
  ]);
in
emacsWithPackages