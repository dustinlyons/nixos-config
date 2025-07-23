;; -------------------------
;; Variable Declarations
;; -------------------------
(defvar org-config-file 
  (or (getenv "EMACS_CONFIG_ORG")  ; Allow override via environment variable
      "~/.config/emacs/config.org"))
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
  (setq use-package-always-ensure t)
  (require 'use-package))

;; -------------------------
;; Environment Variables Setup
;; -------------------------
(unless (package-installed-p 'exec-path-from-shell)
  (package-refresh-contents)
  (package-install 'exec-path-from-shell))

(exec-path-from-shell-initialize)
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

;; Prevent straight.el from byte-compiling f.el to avoid macro expansion issues
(setq straight-disable-byte-compilation '(f))

;; Fix f.el's incorrect :noeval keyword before it's loaded
(defun fix-f-shortdoc ()
  "Fix f.el's shortdoc integration by patching :noeval to :no-eval"
  (let ((f-shortdoc-file (expand-file-name "straight/repos/f.el/f-shortdoc.el" user-emacs-directory)))
    (when (file-exists-p f-shortdoc-file)
      (with-temp-buffer
        (insert-file-contents f-shortdoc-file)
        (goto-char (point-min))
        (while (search-forward ":noeval" nil t)
          (replace-match ":no-eval"))
        (write-region (point-min) (point-max) f-shortdoc-file)))))

;; Apply the fix before f.el is loaded
(fix-f-shortdoc)

;; Fix for f.el shortdoc issue in Emacs 28+ - must be done before loading f.el
(message "DEBUG: Emacs version %s, attempting f.el fix" emacs-version)
(when (>= emacs-major-version 28)
  ;; Force load shortdoc and modify it before f.el can use it
  (message "DEBUG: Loading shortdoc...")
  (require 'shortdoc nil t)
  (message "DEBUG: Shortdoc loaded: %s" (featurep 'shortdoc))
  
  ;; Redefine shortdoc-add-function to handle bad keywords
  (defun shortdoc-add-function (group &rest args)
    "Add function NAME to GROUP.
This version filters out problematic keywords."
    (let ((valid-keywords '(:no-manual :args :eval :result :result-string 
                           :eg-result :eg-result-string :no-eval :no-value
                           :value :result-value :result-prefix :result-postfix
                           :eg-result-prefix :eg-result-postfix :no-eg-result))
          (filtered-args nil)
          (name (car args))
          (props (cdr args)))
      ;; Filter out invalid keywords
      (while props
        (let ((key (car props))
              (val (cadr props)))
          (if (and (keywordp key)
                   (not (memq key valid-keywords))
                   (not (eq key :noeval)))  ; Specifically exclude :noeval
              ;; Skip invalid keyword and its value
              (setq props (cddr props))
            ;; For :noeval, convert to :no-eval
            (if (eq key :noeval)
                (progn
                  (push :no-eval filtered-args)
                  (push val filtered-args)
                  (setq props (cddr props)))
              ;; Keep valid arguments
              (push key filtered-args)
              (when (cdr props)
                (push val filtered-args)
                (setq props (cddr props)))))))
      ;; Add the function with filtered arguments
      (let ((group-info (assq group shortdoc--groups)))
        (unless group-info
          (setq shortdoc--groups (cons (list group) shortdoc--groups))
          (setq group-info (assq group shortdoc--groups)))
        (let ((fun-list (cons name (nreverse filtered-args))))
          (setcdr group-info (append (cdr group-info) (list fun-list)))))))
  
  ;; Also clean up any existing f.el entries
  (with-eval-after-load 'shortdoc
    (when (boundp 'shortdoc--groups)
      (setq shortdoc--groups
            (cl-remove-if (lambda (group) (eq (car group) 'f))
                          shortdoc--groups)))))

;; Load org early to prevent version mismatch
(straight-use-package 'org)

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
  :straight nil  ; Use built-in org-mode, don't install via straight
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