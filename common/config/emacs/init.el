(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

;; use-package package provides common package import functions
(unless (package-installed-p 'use-package)
  (package-initialize)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; This sets up straight.el, a git package manager
(defvar bootstrap-version)
(let ((bootstrap-file
	(expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
	"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Tells (use-package) to use straight.el to download packages
;; straight.el uses git packages, instead of the default bin files, which we like
;; as it's much easier to open it up and hack it
(setq straight-use-package-by-default t)

(defun dl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)) 

(use-package org
  :defer t
  :hook (org-mode . dl/org-mode-setup)
  :config
  (setq org-edit-src-content-indentation 2 ;; Indent code blocks by 2
        org-ellipsis " ▾" ;; Prettify the fold indicator
        org-hide-emphasis-markers t ;; Hide special characters
        org-hide-block-startup nil) ;; Don't start org mode with blocks folded
  :bind
  (("C-c a" . org-agenda)))

(package-initialize)
(require 'org-install)
(org-babel-load-file "~/State/Projects/Code/nixos-config/common/config/emacs/Emacs.org") 
