;; -------------------------
;; Variable Declarations
;; -------------------------
(defvar org-config-file "~/.local/share/src/nixos-config/modules/shared/config/emacs/config.org")
(defvar default-config-file "~/.emacs.d/default-config.org")
(defvar default-config-url "https://raw.githubusercontent.com/dustinlyons/nixos-config/9ad810c818b895c1f67f4daf21bbef31d8b5e8cd/shared/config/emacs/config.org")

;; -------------------------
;; Package Manager Setup
;; -------------------------
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(unless (assoc-default "melpa" package-archives)
  (message "Warning: MELPA source not found. Adding MELPA to package-archives.")
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (message "Warning: Org source not found. Adding Org to package-archives.")
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(setq package-enable-at-startup nil)

;; -------------------------
;; Use-Package Setup
;; -------------------------
(unless (package-installed-p 'use-package)
  (package-initialize)
  (if (package-install 'use-package)
      (message "use-package installed successfully.")
    (error "Error: Failed to install use-package."))
  (setq use-package-verbose t)
  (setq use-package-always-ensure t)
  (require 'use-package))

;; -------------------------
;; Environment Variables Setup
;; -------------------------
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PNPM_HOME"))
  (if (exec-path-from-shell-initialize)
      (message "Environment variables initialized successfully.")
    (error "Error: Failed to initialize environment variables.")))

(when (daemonp)
  (exec-path-from-shell-initialize))

;; -------------------------
;; Straight.el Setup
;; -------------------------
(setq straight-repository-branch "develop")
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
  (if (load bootstrap-file nil 'nomessage)
      (message "Straight.el loaded successfully.")
    (error "Error: Failed to load Straight.el.")))

(setq straight-use-package-by-default t)
(package-initialize)

;; -------------------------
;; Window and UI Setup
;; -------------------------
(defun dl/window-setup ()
  (condition-case nil
      (progn
        (column-number-mode)
        (scroll-bar-mode 0)
        (menu-bar-mode -1)
        (tool-bar-mode 0)
        (winner-mode 1)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark))
        (setq ns-use-proxy-icon nil)
        (setq frame-title-format nil)
        (message "Window and UI setup completed successfully."))
    (error (message "Error occurred in Window and UI setup."))))
(dl/window-setup)

;; -------------------------
;; Org Mode Setup
;; -------------------------
(defun dl/org-mode-setup ()
  (condition-case nil
      (progn
        (org-indent-mode)
        (variable-pitch-mode 1)
        (auto-fill-mode 0)
        (visual-line-mode 1)
        (setq evil-auto-indent nil)
        (message "Org mode setup completed successfully."))
    (error (message "Error occurred in Org mode setup."))))

(use-package org
  :defer t
  :hook (org-mode . dl/org-mode-setup)
  :config
  (setq org-edit-src-content-indentation 2
        org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-hide-block-startup nil)
  :bind (("C-c a" . org-agenda)))

;; -------------------------
;; Default Config Download
;; -------------------------
(defun dl/download-default-config ()
  (condition-case nil
      (progn
        (unless (file-exists-p default-config-file)
          (url-retrieve default-config-url
                        (lambda (_status)
                          ;; delete-region removes the HTTP headers from the downloaded content.
                          (delete-region (point-min) (1+ url-http-end-of-headers))
                          ;; save the contents of the buffer to the file.
                          (write-file default-config-file)))
          (message "Default configuration downloaded successfully.")))
    (error (message "Error occurred while downloading the default configuration."))))

;; -------------------------
;; Load Org Config or Default
;; -------------------------
(condition-case nil
    (progn
      (unless (file-exists-p org-config-file)
        (dl/download-default-config))
      (if (file-exists-p org-config-file)
          (org-babel-load-file org-config-file)
        (org-babel-load-file default-config-file))
      (message "Configuration loaded successfully."))
  (error (message "Error occurred while loading the configuration.")))
