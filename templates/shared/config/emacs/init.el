; setup package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

; turn this off, can conflict with straight.el
(setq package-enable-at-startup nil)

;; use-package package provides common package import functions
;; fetch the list of packages available
(unless (package-installed-p 'use-package)
  (package-initialize)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; Copy $PATH from our environment to Emacs process
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PNPM_HOME"))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

;; This sets up straight.el, a git package manager
;; We use it under the hood of use-package
;; This lets us clone git repos easily if we so choose
(defvar bootstrap-version)
(let ((bootstrap-file
  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(package-initialize)

; configure the initial Emacs window
(defun dl/window-setup ()
  (column-number-mode)
  (scroll-bar-mode 0)
  (menu-bar-mode -1)
  (tool-bar-mode 0)
  (winner-mode 1)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))

; configure org-mode here, so we can use it in config.org
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

; set the window and load the config file
(dl/window-setup)

"Load our main config file"
(org-babel-load-file  "~/.local/share/src/nixos-config/shared/config/emacs/config.org")
