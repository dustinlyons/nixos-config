{ ... }:

{
  # Initializes Emacs with org-mode so we can tangle the main config
  ".emacs.d/init.el".text = builtins.readFile ../common/config/emacs/init.el;
}
