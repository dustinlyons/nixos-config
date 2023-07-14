{ ... }:

{
  # Initializes Emacs with org-mode so we can tangle the main config
  #
  # @todo: Get rid of this after we've upgraded to Emacs 29 on the Macbook
  # Emacs 29 includes org-mode now
  ".emacs.d/init.el" = {
    text = builtins.readFile ../common/config/emacs/init.el;
  };

  # Used in Emacs config.org to load projects and tasks in org-agenda
  ".emacs.d/agenda.txt" = {
    text = ''
      ~/.local/share/org-roam/20220419121404-todo.org
      ~/.local/share/org-roam/20230712154159-health.org
      ~/.local/share/org-roam/20230712154441-family_social.org
      ~/.local/share/org-roam/20230712154303-business.org
      ~/.local/share/org-roam/20210919225144-home_lab.org
    '';
  };
}
