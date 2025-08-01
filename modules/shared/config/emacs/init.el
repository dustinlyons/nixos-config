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
(package-initialize)

;; -------------------------
;; Use-Package Setup
;; -------------------------
(unless (package-installed-p 'use-package)
  (package-initialize)
  (if (package-install 'use-package)
      (message "use-package installed successfully.")
    (error "Error: Failed to install use-package."))
  (setq use-package-verbose t)
  ;; Disable auto-installation since Nix handles packages
  (setq use-package-always-ensure nil)
  (require 'use-package))

;; -------------------------
;; Environment Variables Setup
;; -------------------------
;; exec-path-from-shell is now managed by Nix
(when (require 'exec-path-from-shell nil t)
  (exec-path-from-shell-initialize))
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "PNPM_HOME"))
  (if (exec-path-from-shell-initialize)
      (message "Environment variables initialized successfully.")
    (error "Error: Failed to initialize environment variables.")))

(when (daemonp)
  (exec-path-from-shell-initialize))

;; No need for straight.el anymore - all packages managed by Nix

;; -------------------------
;; Window and UI Setup
;; -------------------------
(defun system-is-mac ()
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  "Return true if system is linux-based"
  (string-equal system-type "gnu/linux"))

(defun dl/window-setup ()
  (condition-case nil
      (progn
        (column-number-mode)
        (scroll-bar-mode 0)
        (menu-bar-mode -1)
        (tool-bar-mode 0)
        (winner-mode 1)
        ;; Only apply macOS-specific settings on macOS
        (when (system-is-mac)
          (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
          (add-to-list 'default-frame-alist '(ns-appearance . dark))
          (setq ns-use-proxy-icon nil))
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
  :ensure nil  ; Use built-in org-mode
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
(condition-case err
    (progn
      (unless (file-exists-p org-config-file)
        (dl/download-default-config))
      (if (file-exists-p org-config-file)
          (org-babel-load-file org-config-file)
        (org-babel-load-file default-config-file))
      (message "Configuration loaded successfully."))
  (error 
    (message "Error occurred while loading the configuration: %s" (error-message-string err))
    ;; Try to at least enable evil-mode if it's available
    (when (fboundp 'evil-mode)
      (evil-mode 1))
    (when (fboundp 'general-create-definer)
      (general-create-definer dl/leader-keys
        :keymaps '(normal visual emacs)
        :prefix ","))))
