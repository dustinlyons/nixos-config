(setq user-full-name "Dustin Lyons"
  user-mail-address "dustin@dlyons.dev")

;; Turn off the splash screen
(setq inhibit-startup-screen t)
;; Turn off the splash screen
(setq initial-scratch-message nil)
;; Confirm before exiting Emacs
(setq confirm-kill-emacs #'yes-or-no-p)
;; Set default frame size and position

(defun adjust-frame-size-and-position (&optional frame)
  "Adjust size and position of FRAME based on its type."
  (if (display-graphic-p frame)
      (let* ((w 120)  ; Set to desired width in characters (good for documents)
            (h 50)   ; Set to desired height in lines (comfortable reading height)
            (width (* w (frame-char-width frame)))
            (height (* h (frame-char-height frame)))
            (left (max 0 (floor (/ (- (x-display-pixel-width) width) 2))))
            (top (max 0 (floor (/ (- (x-display-pixel-height) height) 2)))))

        (set-frame-size frame w h)
        (set-frame-position frame left top))
    ;; Ensure the menu bar is off in terminal mode
    (when (and (not (display-graphic-p frame))
              (menu-bar-mode 1))
      (menu-bar-mode -1))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (adjust-frame-size-and-position frame)))
  (adjust-frame-size-and-position))

;; Smooth out garbage collection
(use-package gcmh
  :ensure nil  ; Managed by Nix
  :demand t
  :config
  (gcmh-mode 1))

;; Add internal borders and fringes for better spacing
(setq-default left-fringe-width 16)
(setq-default right-fringe-width 16)

;; Set frame dimensions in default-frame-alist for consistency
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 50))

;; Set internal border width for padding around the frame
(add-to-list 'default-frame-alist '(internal-border-width . 15))

;; Apply to existing frames
(dolist (frame (frame-list))
  (set-frame-parameter frame 'internal-border-width 15))

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

(defun system-is-mac ()
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  "Return true if system is GNU/Linux-based"
  (string-equal system-type "gnu/linux"))

;; Set path for darwin
(when (system-is-mac)
  (setenv "PATH" (concat (getenv "PATH") ":/Users/dustin/.nix-profile/bin:/usr/bin"))
  (setq exec-path (append '("/Users/dustin/bin" "/profile/bin" "/Users/dustin/.npm-packages/bin" "/Users/dustin/.nix-profile/bin" "/nix/var/nix/profiles/default/bin" "/usr/local/bin" "/usr/bin") exec-path)))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
    ("C-x b" . counsel-ibuffer)
    ("C-x C-f" . counsel-find-file)
    ("C-M-j" . counsel-switch-buffer)
  :map minibuffer-local-map
    ("C-r" . 'counsel-minibuffer-history))
  :custom
    (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
    (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package prescient
  :config
    (prescient-persist-mode 1))

(use-package ivy
  :bind (("C-s" . swiper-all)
  :map ivy-minibuffer-map
    ("TAB" . ivy-partial-or-done)
    ("C-f" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
  :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
  :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill))
  :init
    (ivy-mode 1)
  :config
    (setq ivy-use-virtual-buffers t)
    (setq ivy-wrap t)
    (setq ivy-count-format "(%d/%d) ")
    (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package ivy-prescient
  :after ivy
  :custom
    (prescient-save-file "~/.emacs.d/prescient-data")
    (prescient-filter-method 'fuzzy)
  :config
    (ivy-prescient-mode t))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; ESC will also cancel/quit/etc.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
  :config
    (general-evil-setup t)
    (general-create-definer dl/leader-keys
      :keymaps '(normal visual emacs)
      :prefix ","))

(dl/leader-keys
  "k"  '(:ignore k :which-key "cleanup")
  "ko" '(kill-buffer-and-window :which-key "kill buffer and window")
  "kk" '(kill-some-buffers :which-key "cleanup buffers"))
(global-set-key (kbd "C-x -") 'kill-buffer-and-window)

(dl/leader-keys
  "t"  '(:ignore t :which-key "treemacs")
  "tt" '(treemacs :which-key "toggle treemacs")
  "tx" '(treemacs-collapse-all-projects :which-key "collapse projects")
  "to" '(treemacs-select-window :which-key "select treemacs")
  "tw" '(treemacs-toggle-fixed-width :which-key "size treemacs"))

(dl/leader-keys
  "h" '(counsel-load-theme :which-key "choose theme"))

;; Rotates windows and layouts
(use-package rotate
  :config)

(dl/leader-keys
  "r"   '(:ignore t :which-key "rotate")
  "rw"  '(rotate-window :which-key "rotate window")
  "rl"  '(rotate-layout :which-key "rotate layout"))

;; Turn off UI junk
;; Note to future self: If you have problems with these later,
;; move these into custom file and set variable custom-file
(column-number-mode)
(scroll-bar-mode 0)
(menu-bar-mode -1)
(tool-bar-mode 0)
(winner-mode 1) ;; ctrl-c left, ctrl-c right for window undo/redo

(defun dl/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
      visual-fill-column-center-text t))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dl/org-mode-visual-fill))

(blink-cursor-mode -1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defvar dl/black-color "#1F2528")
(defvar dl/red-color "#EC5F67")
(defvar dl/yellow-color "#FAC863")
(defvar dl/blue-color "#6699CC")
(defvar dl/green-color "#99C794")
(defvar dl/purple-color "#C594C5")
(defvar dl/teal-color "#5FB3B3")
(defvar dl/light-grey-color "#C0C5CE")
(defvar dl/dark-grey-color "#65737E")

;; Run M-x all-the-icons-install-fonts to install
(use-package all-the-icons)

;; f.el - modern file API
(use-package f
  :ensure nil  ; Managed by Nix
  :demand t)

(use-package doom-modeline
  :ensure nil  ; Managed by Nix
  :after f
  :init (doom-modeline-mode 1))

(use-package treemacs
  :config
    (setq treemacs-is-never-other-window 1)
  :bind
    ("C-c t" . treemacs-find-file)
    ("C-c b" . treemacs-bookmark))

(use-package treemacs-icons-dired)
(use-package treemacs-all-the-icons)
(use-package treemacs-projectile)
(use-package treemacs-magit)
(use-package treemacs-evil)

;; Remove binding for facemap-menu, use for ace-window instead
(global-unset-key (kbd "M-o"))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
    (aw-scope 'frame)
    (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (aw-minibuffer-flag t)
  :config
    (ace-window-display-mode 1))

(save-place-mode 1)
(setq save-place-file "~/.local/state/emacs/saveplace")

(savehist-mode 1)
(setq savehist-additional-variables
  '(search-ring
    regexp-search-ring
    kill-ring
    register-alist
    org-refile-history
    org-capture-history))
(setq savehist-file "~/.local/state/emacs/savehist")

(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-saved-items 100
    recentf-max-menu-items 50
    recentf-save-file "~/.local/state/emacs/recentf")
  :config
    (recentf-mode 1))
    (global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Set the default pitch face
(when (system-is-linux)
  (set-face-attribute 'default nil :font "JetBrainsMono" :height 100))
(when (system-is-mac)
  (set-face-attribute 'default nil :font "JetBrains Mono" :height 140))

;; Set the fixed pitch face
(when (system-is-linux)
  (set-face-attribute 'fixed-pitch nil :font "JetBrainsMono" :weight 'normal :height 100))
(when (system-is-mac)
  (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :weight 'normal :height 150))

;; Set the variable pitch face
(when (system-is-linux)
  (set-face-attribute 'variable-pitch nil :font "Helvetica LT Std Condensed" :weight 'normal :height 140))
(when (system-is-mac)
  (set-face-attribute 'variable-pitch nil :font "Helvetica" :weight 'normal :height 170))

(use-package dashboard
  :ensure nil  ; Managed by Nix
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'ascii
        dashboard-center-content t
        dashboard-items '((projects . 5)
                           (recents  . 5)))
  (setq dashboard-set-footer nil))

  (setq dashboard-banner-logo-title "This is your life")
  (setq dashboard-set-file-icons t)
  (setq dashboard-projects-backend 'projectile)

  (setq initial-buffer-choice (lambda ()
                                  (get-buffer-create "*dashboard*")
                                  (dashboard-refresh-buffer)))
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)

(setq-default indent-tabs-mode nil
            js-indent-level 2
            tab-width 2)
(setq-default evil-shift-width 2)

(global-set-key (kbd "<C-tab>") 'next-buffer)

(use-package doom-themes
  :ensure nil  ; Managed by Nix
  :config
    (setq doom-themes-enable-bold t
            doom-themes-enable-italic t)
    (load-theme 'doom-1337 t)
    
    ;; Fix white frame issue by setting default frame parameters
    (setq default-frame-alist 
          (append default-frame-alist
                  '((background-color . "#1c1e27")
                    (foreground-color . "#cccac2"))))
    (set-face-background 'default "#1c1e27")
    (set-face-background 'fringe "#1c1e27")
    (set-face-background 'internal-border "#1c1e27")
    
    ;; Add line spacing for better readability
    (setq-default line-spacing 0.15)
    
    (doom-themes-visual-bell-config)
    (doom-themes-org-config))

(defalias 'yes-or-no-p 'y-or-n-p) ;; Use Y or N in prompts, instead of full Yes or No

(global-visual-line-mode t) ;; Wraps lines everywhere
(global-auto-revert-mode t) ;; Auto refresh buffers from disk
(line-number-mode t) ;; Line numbers in the gutter
(show-paren-mode t) ;; Highlights parans for me

(setq warning-minimum-level :error)

(setq org-agenda-files "~/.emacs.d/agenda.txt" )
(setq org-archive-location "~/.local/share/org-roam/20220318132418-archive.org::")

(defun my-org-insert-subheading (heading-type)
"Inserts a new org heading with unique ID and creation date.
The type of heading (TODO, PROJECT, etc.) is specified by HEADING-TYPE."
  (let ((uuid (org-id-uuid))
        (date (format-time-string "[%Y-%m-%d %a %H:%M]")))
    (org-end-of-line) ;; Make sure we are at the end of the line
    (unless (looking-at-p "\n") (insert "\n")) ;; Insert newline if next character is not a newline
    (org-insert-subheading t) ;; Insert a subheading instead of a heading
    (insert (concat heading-type " "))
    (save-excursion
      (org-set-property "ID" uuid)
      (org-set-property "CREATED" date))))

(defun my-org-insert-todo ()
  "Inserts a new TODO heading with unique ID and creation date."
  (interactive)
  (my-org-insert-subheading "TODO"))

(defun my-org-insert-project ()
  "Inserts a new PROJECT heading with unique ID and creation date."
  (interactive)
  (my-org-insert-subheading "PROJECT"))

(defun my-org-copy-link-from-id ()
  "Copies a link to the current Org mode item by its ID to clipboard"
  (interactive)
  (when (org-at-heading-p)
    (let* ((element (org-element-at-point))
           (title (org-element-property :title element))
           (id (org-entry-get nil "ID"))
           (link (format "[[id:%s][%s]]" id title)))
      (when id
        (kill-new link)
        (message "Link saved to clipboard")))))

(define-prefix-command 'my-org-todo-prefix)

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t") 'my-org-todo-prefix)

(define-key 'my-org-todo-prefix (kbd "t") 'my-org-insert-todo)
(define-key 'my-org-todo-prefix (kbd "p") 'my-org-insert-project)

(define-key org-mode-map (kbd "C-c l") 'my-org-copy-link-from-id)

;; Fast access to tag common contexts I use
(setq org-todo-keywords
 '((sequence "TODO(t)" "STARTED(s)" "WAITING(w@/!)"
             "DELEGATED(g@/!)" "DEFERRED(r)" "SOMEDAY(y)"
             "|" "DONE(d@)" "CANCELED(x@)")
   (sequence "PROJECT(p)" "|" "DONE(d@)" "CANCELED(x@)")
   (sequence "APPT(a)" "|" "DONE(d@)" "CANCELED(x@)")))

(setq org-todo-keyword-faces
  `(("TODO" . ,dl/green-color)
    ("STARTED" . ,dl/yellow-color)
    ("WAITING" . ,dl/light-grey-color)
    ("DELEGATED" . ,dl/teal-color)
    ("DEFERRED" . ,dl/dark-grey-color)
    ("SOMEDAY" . ,dl/purple-color)
    ("DONE" . ,dl/dark-grey-color)
    ("CANCELED" . ,dl/dark-grey-color)
    ("PROJECT" . ,dl/blue-color)
    ("APPT" . ,dl/green-color)))

(defface my-org-agenda-face-1-2
  '((t (:inherit default :height 1.2)))
  "Face for org-agenda mode.")

(defun my-set-org-agenda-font ()
  "Set the font for `org-agenda-mode'."
  (buffer-face-set 'my-org-agenda-face-1-2))

(add-hook 'org-agenda-mode-hook 'my-set-org-agenda-font)

(setq display-buffer-alist
    `((".*Org Agenda.*"
       (display-buffer-below-selected)
       (inhibit-same-window . t)
       (window-height . 0.5))))

(defun dl/buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                            (point-max) t)
      (buffer-substring-no-properties
      (match-beginning 1)
      (match-end 1)))))

(defun dl/agenda-category (&optional len)
  "Get category of item at point for agenda."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                      (file-name-nondirectory buffer-file-name))))
        (title (dl/buffer-prop-get "title"))
        (category (org-get-category))
        (result (or (if (and title (string-equal category file-name))
                        title
                      category))))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(setq org-agenda-hide-tags-regexp (regexp-opt '("Todo" "home" "work")))

(setq org-agenda-prefix-format
      '((agenda . " %i %(dl/agenda-category 12)%?-32t% s")
        (todo . " %i %(dl/agenda-category 32) ")
        (tags . " %i %(dl/agenda-category 32) ")
        (search . " %i %(dl/agenda-category 32) ")))

(evil-set-initial-state 'org-agenda-mode 'normal)
(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
  (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line))

(setq org-agenda-todo-ignore-keywords '("PROJECT"))

(use-package org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-dim-blocked-tasks nil))

;; Define custom faces for group highlighting
(defface org-super-agenda-header
  '((t (:inherit org-agenda-structure :height 1.1 :foreground "#7cc3f3" :background "#282c34")))
  "Face for highlighting org-super-agenda groups.")

(defface org-super-agenda-subheader
  '((t (:inherit org-agenda-structure :height 1.0 :foreground "light slate gray" :background "black")))
  "Face for highlighting org-super-agenda subgroups.")

;; Apply the custom faces to org-super-agenda
(custom-set-faces
 '(org-super-agenda-header ((t (:inherit org-agenda-structure :height 1.1 :foreground "#7cc3f3" :background "#282c34"))))
 '(org-super-agenda-subheader ((t (:inherit org-agenda-structure :height 1.0 :foreground "light slate gray" :background "black")))))

(setq org-super-agenda-groups
  '((:name "Priority A"
     :priority "A")
    (:name "Priority B"
     :priority "B")
    (:name "Priority C"
     :priority "C")
    (:name "Started"
     :todo "STARTED")
    (:name "Waiting"
     :todo "WAITING")
    (:name "Tasks"
     :todo "TODO")
    (:name "Learn"
     :tag "learn")
    (:name "Someday"
     :todo "SOMEDAY")
  (:name "Projects"
   :tag "PROJECT")))

(org-super-agenda-mode)

(use-package org-transclusion
  :after org
  :hook (org-mode . org-transclusion-mode))

(defun org-global-props (&optional property buffer)
  "Helper function to grab org properties"
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword
    (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

;; Removed dl/refile-and-transclude function - no longer needed

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun dl/find-file (path)
  "Helper function to open a file in a buffer"
  (interactive)
  (find-file path))

(defun dl/load-buffer-with-emacs-config ()
  "Open the emacs configuration"
  (interactive)
  (dl/find-file "~/.local/share/src/nixos-config/modules/shared/config/emacs/config.org"))

(defun dl/load-buffer-with-nix-config ()
  "Open the emacs configuration"
  (interactive)
  (dl/find-file "~/.local/share/src/nixos-config/modules/shared/home-manager.nix"))

(defun dl/reload-emacs ()
  "Reload the emacs configuration"
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun dl/insert-header ()
  "Insert a header indented one level from the current header, unless the current header is a timestamp."
  (interactive)
  (let* ((level (org-current-level))
        (headline (org-get-heading t t t t))
        (next-level (if (string-match "^\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)" headline)
                        (1+ level)
                      level)))
    (end-of-line)
    (newline)
    (insert (make-string next-level ?*))
    (insert " ")))

(defun dl/insert-current-time ()
  "Insert the current time into the current buffer, at a level one deeper than the current heading."
  (interactive)
  (let* ((level (org-current-level))
         (next-level (1+ level)))
    (end-of-line)
    (newline)
    (insert (make-string next-level ?*))
    (insert " " (format-time-string "%H:%M:%S" (current-time)) "\n")))

"Emacs relates shortcuts"
(dl/leader-keys
  "e"  '(:ignore t :which-key "emacs")
  "ee" '(dl/load-buffer-with-emacs-config :which-key "open emacs config")
  "er" '(dl/reload-emacs :which-key "reload emacs"))

(use-package yasnippet)
(yas-global-mode 1)

(setq org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
    :unnarrowed t)))

(use-package org-roam
  :ensure nil  ; Managed by Nix
  :init
    (setq org-roam-v2-ack t) ;; Turn off v2 warning
    ;; Use the builtin SQLite backend - set this before loading org-roam
    (setq org-roam-database-connector 'sqlite-builtin)
  :demand t  ; Load immediately to ensure settings take effect
  :custom
    (org-roam-directory (file-truename "~/.local/share/org-roam"))
    (org-roam-dailies-directory "daily/")
    (org-roam-completion-everywhere t)
  :config
    (add-to-list 'display-buffer-alist
         '("\\*org-roam\\*"
           (display-buffer-in-direction)
           (direction . right)
           (window-width . 0.33)
           (window-height . fit-window-to-buffer)))
    (org-roam-db-autosync-mode)
  :bind
    (("C-c r b" . org-roam-buffer-toggle)
     ("C-c r t" . org-roam-dailies-goto-today)
     ("C-c r y" . org-roam-dailies-goto-yesterday)
     ("C-c r n" . org-roam-node-insert)
     ("C-c r f" . org-roam-node-find)
     ("C-c r c" . dl/org-roam-create-id)
     ("M-s-n" . org-roam-node-insert)  ; Alt + Super + N to create new node
     ("M-s-f" . org-roam-node-find)    ; Alt + Super + F to find node
       :map org-mode-map
     ("C-M-i"   . completion-at-point)
     ("C-<left>" . org-roam-dailies-goto-previous-note)
     ("C-<right>" . org-roam-dailies-goto-next-note)))

(setq org-roam-dailies-capture-templates
  '(("d" "default" entry
     "* %?"
     :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n#+filetags: Daily\n\n"))))

(defvar dl/org-created-property-name "CREATED")

(defun dl/org-set-created-property (&optional active name)
  (interactive)
  (let* ((created (or name dl/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now)
      now)))

(defun dl/org-find-time-file-property (property &optional anywhere)
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading) t)
        (point)))))

(defun dl/org-has-time-file-property-p (property &optional anywhere)
  (when-let ((pos (dl/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos -1))))

(defun dl/org-set-time-file-property (property &optional anywhere pos)
  (when-let ((pos (or pos
                      (dl/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun dl/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (dl/org-set-time-file-property "LAST_MODIFIED")))

(defun dl/org-roam-create-id ()
"Add created date to org-roam node."
  (interactive)
  (org-id-get-create)
  (dl/org-set-created-property))

(set-face-attribute 'ivy-current-match nil :foreground "#3d434d" :background "#ffcc66")

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("•" "•" "•" "◦" "◦" "◦" "◦")))

(add-hook 'org-mode-hook 'variable-pitch-mode)
 (require 'org-indent)
 (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
 (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
 (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
 (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
 (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
 (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
 (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
 (when (system-is-linux)
   (set-face-attribute 'org-document-title nil :font "Helvetica LT Std Condensed" :weight 'bold :height 1.2))
 (when (system-is-mac)
   (set-face-attribute 'variable-pitch nil :font "Helvetica" :height 120))
 (dolist (face '((org-level-1 . 1.2)
                 (org-level-2 . 1.15)
                 (org-level-3 . 1.1)
                 (org-level-4 . 1.05)
                 (org-level-5 . 1.05)
                 (org-level-6 . 1.0)
                 (org-level-7 . 1.0)
                 (org-level-8 . 1.0)))
(when (system-is-linux)
  (set-face-attribute (car face) nil :font "Helvetica LT Std Condensed" :weight 'medium :height (cdr face)))
(when (system-is-mac)
  (set-face-attribute 'variable-pitch nil :font "Helvetica" :weight 'medium :height 170)))

(defun dl/evil-hook ()
  (dolist (mode '(eshell-mode
                  git-rebase-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode))) ;; no evil mode for these modes

(use-package evil
  :init
    (setq evil-want-integration t) ;; TODO: research what this does
    (setq evil-want-keybinding nil) ;; Required for evil-collection
    (setq evil-want-fine-undo 'fine) ;; undo/redo each motion
    (setq evil-want-Y-yank-to-eol t) ;; Y copies to end of line like vim
    (setq evil-want-C-u-scroll t) ;; vim like scroll up
  :config
    (evil-mode 1)
    (dl/evil-hook)
    ;; Emacs "cancel" == vim "cancel"
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

    ;; Ctrl-h deletes in vim insert mode
    (define-key evil-insert-state-map (kbd "C-h")
      'evil-delete-backward-char-and-join)

    ;; When we wrap lines, jump visually, not to the "actual" next line
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'message-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

  ;; Gives me vim bindings elsewhere in emacs
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  ;; Keybindings in org mode
  (use-package evil-org
    :after evil
    :hook
      (org-mode . (lambda () evil-org-mode))
    :config
      (require 'evil-org-agenda)
      (evil-org-agenda-set-keys))

  ;; Branching undo system
  (use-package undo-tree
    :after evil
    :diminish
    :config
    (evil-set-undo-system 'undo-tree)
    (global-undo-tree-mode 1))

  (use-package evil-commentary
    :after evil
    :config
    (evil-commentary-mode))

  ;; Keep undo files from littering directories
  (setq undo-tree-history-directory-alist '(("." . "~/.local/state/emacs/undo")))

(use-package all-the-icons-dired)

(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :config
    (setq dired-listing-switches "-agho --group-directories-first")
    (setq dired-omit-files "^\\.[^.].*")
    (setq dired-omit-verbose nil)
    (setq dired-hide-details-hide-symlink-targets nil)
    (put 'dired-find-alternate-file 'disabled nil)
    (setq delete-by-moving-to-trash t)
    (autoload 'dired-omit-mode "dired-x")
    (add-hook 'dired-load-hook
          (lambda ()
            (interactive)
            (dired-collapse)))
    (add-hook 'dired-mode-hook
          (lambda ()
            (interactive)
            (dired-omit-mode 1)
            (dired-hide-details-mode 1)
            (all-the-icons-dired-mode 1))
            (hl-line-mode 1)))

(use-package dired-ranger)
(use-package dired-collapse)

(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-up-directory
  "c" 'find-file
  "H" 'dired-omit-mode
  "l" 'dired-find-file
  "o" 'dired-find-file-other-window
  (kbd "C-o") 'dired-display-file
  "y" 'dired-ranger-copy
  "X" 'dired-ranger-move
  "p" 'dired-ranger-paste
  "gr" 'revert-buffer
  "g" 'revert-buffer
  (kbd "RET") 'dired-find-file
  (kbd "<return>") 'dired-find-file)

;; Darwin needs ls from coreutils for dired to work
(when (system-is-mac)
  (setq insert-directory-program
    (expand-file-name ".nix-profile/bin/ls" (getenv "HOME"))))

(defun my-org-archive-done-tasks ()
  "Archive all DONE tasks in the current buffer."
  (interactive)
  (org-map-entries
  (lambda ()
    (org-archive-subtree)
    (setq org-map-continue-from (outline-previous-heading)))
  "/DONE" 'tree))

(defun er-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (yes-or-no-p (concat "Do you really want to delete file: " filename "? ")) ; Ask for confirmation
          (if (vc-backend filename)
              (vc-delete-file filename)
            (progn
              (delete-file filename)
              (message "Deleted file %s" filename)
              (kill-buffer)))
        (message "Aborted"))))) ; Abort message

(define-key org-mode-map (kbd "C-c D") 'my-org-archive-done-tasks)
(define-key org-mode-map (kbd "C-c d") 'org-archive-subtree)
(global-set-key (kbd "C-c x")  #'er-delete-file-and-buffer)

(use-package org-download
  :after org
  :custom
  (org-download-image-dir (expand-file-name "files" "~/.local/share/org-roam/"))
  :hook
  (dired-mode . org-download-enable))

(setq backup-directory-alist
      `((".*" . "~/.local/state/emacs/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t) ; Automatically delete excess backups

(setq auto-save-file-name-transforms
      `((".*" "~/.local/state/emacs/" t)))
(setq lock-file-name-transforms
      `((".*" "~/.local/state/emacs/lock-files/" t)))

(use-package deadgrep
  :bind (("C-c d g" . deadgrep)
         ("C-c d p" . deadgrep-project))
  :config
  (defun deadgrep-project ()
    "Run deadgrep in the current project root."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (call-interactively #'deadgrep)))
  
  ;; Customize deadgrep faces for better visibility
  :custom-face
  ;; File headers with background color
  (deadgrep-filename-face ((t (:foreground "#7cc3f3" 
                                :background "#2a2e38"
                                :weight bold
                                :height 1.1
                                :box (:line-width 2 :color "#3a3e48")))))
  ;; Match count styling
  (deadgrep-match-count-face ((t (:foreground "#99C794"
                                   :weight bold))))
  ;; Search term highlighting
  (deadgrep-match-face ((t (:foreground "#1c1e27"
                             :background "#FAC863"
                             :weight bold))))
  ;; Line numbers
  (deadgrep-line-number-face ((t (:foreground "#65737E"))))
  ;; Meta information
  (deadgrep-meta-face ((t (:foreground "#5FB3B3"
                           :italic t)))))

;; Leader key bindings for Deadgrep
(dl/leader-keys
  "sd"  '(:ignore t :which-key "deadgrep")
  "sdg" '(deadgrep :which-key "deadgrep here")
  "sdp" '(deadgrep-project :which-key "deadgrep project"))

;; Configure key-chord for double-shift to trigger deadgrep
(use-package key-chord
  :ensure nil  ; Managed by Nix
  :config
  (key-chord-mode 1)
  ;; Set the delay between key presses (in seconds)
  (setq key-chord-two-keys-delay 0.2)
  ;; Define double-bracket to run deadgrep
  ;; Note: Shift alone cannot be bound, so we use a workaround
  (key-chord-define-global "[[" 'deadgrep))

;; Since Emacs can't detect shift key alone, we use double brackets
(message "Deadgrep configured: Use double-bracket ([[) to launch deadgrep")

(use-package ripgrep)
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
    ((projectile-completion-system 'ivy))
  :bind-keymap
	    ("C-c p" . projectile-command-map)
  :init
    (setq projectile-enable-caching t)
    (setq projectile-sort-order 'recently-active)
    (setq projectile-switch-project-action #'projectile-dired))

(setq projectile-project-root-files-bottom-up '("package.json" ".projectile" ".project" ".git"))
(setq projectile-ignored-projects '("~/.emacs.d/"))
(setq projectile-globally-ignored-directories '("dist" "node_modules" ".log" ".git"))

;; Custom projectile project shortcuts
(defun dl/open-conductly-project ()
  "Open the Conductly project starting at README.md."
  (interactive)
  (let ((conductly-path (expand-file-name "~/src/conductly/README.md")))
    (if (file-exists-p conductly-path)
        (progn
          (find-file conductly-path)
          (projectile-discover-projects-in-directory "~/src/conductly")
          (projectile-switch-project-by-name "~/src/conductly"))
      (message "Conductly project not found at ~/src/conductly"))))

(defun dl/open-nixos-config-project ()
  "Open the nixos-config project starting at README.md."
  (interactive)
  (let ((nixos-config-path (expand-file-name "~/src/nixos-config/README.md")))
    (if (file-exists-p nixos-config-path)
        (progn
          (find-file nixos-config-path)
          (projectile-discover-projects-in-directory "~/src/nixos-config")
          (projectile-switch-project-by-name "~/src/nixos-config"))
      (message "nixos-config project not found at ~/src/nixos-config"))))

;; Custom magit project functions
(defun dl/magit-status-conductly ()
  "Open magit status for Conductly project in full frame."
  (interactive)
  (let ((conductly-path (expand-file-name "~/src/conductly")))
    (if (file-directory-p conductly-path)
        (progn
          (magit-status conductly-path)
          (delete-other-windows))
      (message "Conductly project not found at ~/src/conductly"))))

(defun dl/magit-status-nixos-config ()
  "Open magit status for nixos-config project in full frame."
  (interactive)
  (let ((nixos-config-path (expand-file-name "~/src/nixos-config")))
    (if (file-directory-p nixos-config-path)
        (progn
          (magit-status nixos-config-path)
          (delete-other-windows))
      (message "nixos-config project not found at ~/src/nixos-config"))))
(defun dl/magit-status-river ()
  "Open magit status for River project in full frame."
  (interactive)
  (let ((river-path (expand-file-name "~/src/river")))
    (if (file-directory-p river-path)
        (progn
          (magit-status river-path)
          (delete-other-windows))
      (message "River project not found at ~/src/river"))))

;; Projectile leader key bindings
(dl/leader-keys
  "p"   '(:ignore t :which-key "projectile")
  "pc"  '(dl/open-conductly-project :which-key "conductly")
  "pn"  '(dl/open-nixos-config-project :which-key "nixos-config")
  "pp"  '(counsel-projectile-switch-project :which-key "switch project")
  "pf"  '(counsel-projectile-find-file :which-key "find file")
  "ps"  '(counsel-projectile-rg :which-key "search project")
  "pb"  '(counsel-projectile-switch-to-buffer :which-key "switch buffer")
  "pd"  '(projectile-dired :which-key "project dired"))

;; Git/Magit leader key bindings
(dl/leader-keys
  "g"   '(:ignore t :which-key "git")
  "gc"  '(dl/magit-status-conductly :which-key "magit conductly")
  "gn"  '(dl/magit-status-nixos-config :which-key "magit nixos-config")
  "gr"  '(dl/magit-status-river :which-key "magit river")
  "gg"  '(magit-status :which-key "magit status")
  "gb"  '(magit-blame :which-key "magit blame")
  "gl"  '(magit-log-buffer-file :which-key "magit log file")
  "gf"  '(forge-dispatch :which-key "forge (PRs/issues)"))

;; Gives me Ivy options in the Projectile menus
(use-package counsel-projectile 
  :after projectile
  :config
  (counsel-projectile-mode 1))

;; Project-wide search keybindings
(defun my/swiper-project ()
  "Search across all files in current project using ripgrep."
  (interactive)
  (counsel-rg nil (projectile-project-root)))

;; Search keybindings for projectile
(dl/leader-keys
  "/"   '(counsel-projectile-rg :which-key "search project")
  "?"   '(my/swiper-project :which-key "search project (alt)")
  "a"   '(:ignore t :which-key "search")
  "aa"  '(swiper-all :which-key "search buffers") 
  "ap"  '(counsel-projectile-rg :which-key "search project")
  "ag"  '(counsel-projectile-grep :which-key "grep project")
  "af"  '(counsel-projectile-find-file :which-key "find file")
  "ad"  '(counsel-projectile-find-dir :which-key "find directory"))

;; Alternative global keybindings for quick access
(global-set-key (kbd "C-c C-s") 'counsel-projectile-rg)
(global-set-key (kbd "C-c s p") 'my/swiper-project)
(global-set-key (kbd "C-c s a") 'swiper-all)

(when (system-is-mac)
  (with-eval-after-load "ispell"
    (setq ispell-program-name
      (expand-file-name ".nix-profile/bin/hunspell" (getenv "HOME")))
    (setq ispell-dictionary "en_US"))
  (setq ispell-personal-dictionary "~/.local/share/dict/user/hunspell_en_US"))

(use-package flyspell-correct
:after flyspell
:bind nil)

(dl/leader-keys
  "s" '(flyspell-correct-wrapper :which-key "correct word"))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(add-hook 'git-commit-mode-hook  'turn-on-flyspell)
(add-hook 'text-mode-hook        'flyspell-mode)
(add-hook 'org-mode-hook         'flyspell-mode)
(add-hook 'prog-mode-hook        'flyspell-prog-mode)

(defun spell() (interactive) (flyspell-mode 1))

;; Auto scroll the buffer as we compile
(setq compilation-scroll-output t)

;; By default, eshell doesn't support ANSI colors. Enable them for compilation.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package tide
  :ensure nil  ; Managed by Nix
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (web-mode . (lambda ()
                       (when (string-match "tsx?" (file-name-extension buffer-file-name))
                         (tide-setup)
                         (tide-hl-identifier-mode))))
         (before-save . tide-format-before-save)))

(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
        :placeOpenBraceOnNewLineForFunctions nil))

(use-package lsp-mode
  :commands lsp lsp-deferred
  :init
    (setq lsp-keymap-prefix "C-c l")
    (setq lsp-restart 'ignore)
    (setq lsp-headerline-breadcrumb-enable nil)
    (setq lsp-auto-guess-root t)
    (setq lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
    (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
        ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
   :custom
     (company-minimum-prefix-length 1)
     (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(add-hook 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)

(defun dl/lsp-find-references-other-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (lsp-find-references))

(defun dl/lsp-find-implementation-other-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (lsp-find-implementation))

(defun dl/lsp-find-definition-other-window ()
  (interactive)
  (switch-to-buffer-other-window (current-buffer))
  (lsp-find-definition))

(dl/leader-keys
"l"  '(:ignore t :which-key "lsp")
"lf" '(dl/lsp-find-references-other-window :which-key "find references")
"lc" '(dl/lsp-find-implementation-other-window :which-key "find implementation")
"ls" '(lsp-treemacs-symbols :which-key "list symbols")
"lt" '(flycheck-list-errors :which-key "list errors")
"lh" '(lsp-treemacs-call-hierarchy :which-key "call hierarchy")
"lF" '(lsp-format-buffer :which-key "format buffer")
"li" '(lsp-organize-imports :which-key "organize imports")
"ll" '(lsp :which-key "enable lsp mode")
"lr" '(lsp-rename :which-key "rename")
"ld" '(dl/lsp-find-definition-other-window :which-key "goto definition"))

(use-package dumb-jump
  :ensure nil  ; Managed by Nix
  :config
  ;; Use ripgrep as the preferred searcher (it's the fastest)
  (setq dumb-jump-prefer-searcher 'rg)
  ;; Be smart about project context
  (setq dumb-jump-force-searcher nil)
  ;; Add to xref backend as a fallback
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; Silence the mode-line indicator
  (setq dumb-jump-mode-line-format nil))

;; Leader key bindings for dumb-jump
(dl/leader-keys
  "j"  '(:ignore t :which-key "jump")
  "jj" '(dumb-jump-go :which-key "jump to definition")
  "jo" '(dumb-jump-go-other-window :which-key "jump other window")
  "jb" '(dumb-jump-back :which-key "jump back")
  "jl" '(dumb-jump-quick-look :which-key "quick look")
  "je" '(dumb-jump-go-prefer-external :which-key "jump external"))

(use-package lsp-pyright
  :ensure nil  ; Managed by Nix
  :hook (python-mode . (lambda ()
    (require 'lsp-pyright)
    (lsp-deferred))))  ; or lsp-deferred

(setq python-indent-offset 2)

(use-package blacken
  :ensure nil)  ; Managed by Nix

(setq blacken-line-length '88)
(setq blacken-allow-py36 t)
(setq blacken-executable "black")
(setq blacken-fast-unsafe t)

(add-hook 'python-mode-hook 'blacken-mode)

(add-to-list 'auto-mode-alist '("\\.env" . shell-script-mode))

(use-package yaml-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.yml\\'" . yaml-mode)))

;; This uses Github Flavored Markdown for README files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config
  ;; Enable syntax highlighting
  (setq markdown-fontify-code-blocks-natively t)
  ;; Enable inline code highlighting
  (setq markdown-enable-highlighting-syntax t)
  ;; Optional: customize faces for better visibility
  (custom-set-faces
   '(markdown-code-face ((t (:inherit fixed-pitch :background "#2d2d2d"))))
   '(markdown-inline-code-face ((t (:inherit (font-lock-constant-face fixed-pitch) :background "#2d2d2d"))))))

(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(define-key emmet-mode-keymap [tab] 'emmet-expand-line)
(add-to-list 'emmet-jsx-major-modes 'jsx-mode)

(use-package rainbow-mode)

(use-package go-mode)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)

(defun dl/go-mode-hook ()
  ; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
  ; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  ; Godef jump key binding
  (local-set-key (kbd "M-.") 'godef-jump)
  ;; pop-tag-mark moves back before jump, to undo M-,
  (local-set-key (kbd "M-*") 'pop-tag-mark))

(add-hook 'go-mode-hook 'dl/go-mode-hook)

(use-package php-mode
  :ensure nil  ; Managed by Nix
  :config
    (add-hook 'php-mode-hook 'lsp-deferred))

;; Enhanced LSP configuration for PHP with PHPStan integration
(use-package lsp-mode
  :config
  ;; Configure Phpactor for better PHP support with refactoring capabilities
  (setq lsp-phpactor-path (executable-find "phpactor"))
  
  ;; Force phpactor to be the only PHP server
  (with-eval-after-load 'lsp-php
    ;; Disable all other PHP clients
    (setq lsp-clients-php-iph-server-command nil)
    (setq lsp-clients-php-server-command nil)
    (setq lsp-disabled-clients '(iph php-ls psalm-ls php-serenata))
    ;; Only enable phpactor
    (setq lsp-enabled-clients '(phpactor)))
  
  ;; Configure lsp-mode file watch threshold if needed
  (setq lsp-file-watch-threshold 2000)
  
  ;; Better completion
  (setq lsp-completion-provider :company-capf)
  (setq company-minimum-prefix-length 2)
  
  ;; Use flycheck for diagnostics to integrate with PHPStan
  (setq lsp-diagnostics-provider :flycheck))

;; Configure flycheck to run PHPStan
(use-package flycheck
  :ensure nil  ; Managed by Nix
  :config
  ;; Define PHPStan checker
  (flycheck-define-checker phpstan
    "PHP static analysis using PHPStan."
    :command ("phpstan" "analyse" "--no-progress" "--error-format=raw" source-original)
    :error-patterns
    ((error line-start (file-name) ":" line ":" (message) line-end))
    :modes php-mode)
  
  ;; Add PHPStan to PHP checkers - run after the built-in PHP checker
  (add-to-list 'flycheck-checkers 'phpstan t)
  (flycheck-add-next-checker 'php 'phpstan))

;; PHP mode setup with flycheck and formatting
(defun setup-php-development ()
  "Setup PHP development environment with LSP, flycheck, and formatting."
  ;; Force phpactor by disabling other clients before starting LSP
  (setq-local lsp-disabled-clients '(iph php-ls))
  (setq-local lsp-enabled-clients '(phpactor))
  (lsp-deferred)
  (flycheck-mode 1)
  ;; Enable PHPStan checking
  (when (executable-find "phpstan")
    (flycheck-select-checker 'phpstan)))

;; Add enhanced PHP mode hook
(add-hook 'php-mode-hook #'setup-php-development)

;; PHP formatting setup
(require 'cl-lib)
(require 'php-cs-fixer)
(add-hook 'before-save-hook 'php-cs-fixer-before-save)

;; PHP navigation and documentation
(defun php-doc-at-point ()
  "Look up PHP documentation for symbol at point."
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol
      (browse-url (format "https://www.php.net/manual/en/function.%s.php" 
                         (replace-regexp-in-string "_" "-" symbol))))))

;; PHP-specific keybindings
(eval-after-load 'php-mode
  '(progn
     (define-key php-mode-map (kbd "C-c C-d") 'php-doc-at-point)
     (define-key php-mode-map (kbd "C-c ! n") 'flycheck-next-error)
     (define-key php-mode-map (kbd "C-c ! p") 'flycheck-previous-error)
     (define-key php-mode-map (kbd "C-c ! l") 'flycheck-list-errors)
     ;; Ctrl+Alt+L to fix PHP code formatting
     (define-key php-mode-map (kbd "C-M-l") 'php-cs-fixer-fix)))

;; Evil mode error navigation for PHP
(with-eval-after-load 'evil
  (evil-define-key 'normal php-mode-map
    "]e" 'flycheck-next-error
    "[e" 'flycheck-previous-error))

;; PHP Leader key bindings
(dl/leader-keys
  "P"  '(:ignore t :which-key "php")
  "Pc" '((lambda () (interactive) (compile "composer install")) :which-key "composer install")
  "Pu" '((lambda () (interactive) (compile "composer update")) :which-key "composer update")
  "Pf" '(php-cs-fixer-fix :which-key "fix code style")
  "Ps" '((lambda () (interactive) 
           (let ((default-directory (projectile-project-root)))
             (compile "phpstan analyse --no-progress"))) :which-key "phpstan project")
  "PS" '((lambda () (interactive) 
           (compile (format "phpstan analyse --no-progress %s" (buffer-file-name)))) :which-key "phpstan file")
  "Pd" '(php-doc-at-point :which-key "php documentation"))

;; Adjust auto-mode-alist to use php-mode for PHP files
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;; Modern tree-sitter support for better syntax highlighting
(use-package tree-sitter
  :ensure nil  ; Managed by Nix
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure nil  ; Managed by Nix
  :after tree-sitter)

;; Use built-in treesit for Emacs 29+ or fallback to tree-sitter
(if (and (fboundp 'treesit-available-p) (treesit-available-p))
    (progn
      ;; Native tree-sitter modes for Emacs 29+
      (use-package typescript-ts-mode
        :mode (("\\.ts\\'" . typescript-ts-mode)
               ("\\.tsx\\'" . tsx-ts-mode))
        :hook ((typescript-ts-mode . lsp-deferred)
               (tsx-ts-mode . lsp-deferred))))
  ;; Fallback to web-mode with enhanced configuration
  (progn
    (use-package web-mode
      :hook (web-mode . lsp-deferred)
      :config
      ;; Set content types for proper syntax highlighting
      (setq web-mode-content-types-alist
            '(("jsx" . "\\.js[x]?\\'")
              ("jsx" . "\\.tsx\\'")))  ; Force TSX to use JSX content type
      ;; Enable syntax highlighting features
      (setq web-mode-enable-auto-pairing t)
      (setq web-mode-enable-css-colorization t)
      (setq web-mode-enable-current-element-highlight t)
      (setq web-mode-enable-auto-quoting nil))
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tsx$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ts$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mjs$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2))
(add-hook 'web-mode-hook  'web-mode-init-hook)

;; TypeScript mode for non-TSX files if not using tree-sitter
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (use-package typescript-mode
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)))

;; Keeps indentation organized across these modes
(use-package prettier-js)

;; Turn off hooks for now 1/4/2024 - DHL
;;(add-hook 'js2-mode-hook 'prettier-js-mode)
;;(add-hook 'web-mode-hook 'prettier-js-mode)
;;(add-hook 'css-mode-hook 'prettier-js-mode)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :config
  ;; Force evil keybindings in magit
  (evil-set-initial-state 'magit-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'magit-diff-mode 'normal)
  (evil-set-initial-state 'magit-log-mode 'normal)
  ;; Ensure evil-collection loads after magit
  (require 'evil-collection)
  (evil-collection-magit-setup)
  ;; Configure magit to display in full frame
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
(define-key magit-hunk-section-map (kbd "RET") 'magit-diff-visit-file-other-window)
(global-set-key (kbd "C-x G") 'magit-log-buffer-file)

;; --- auth-source backend that hands out the gh CLI token ------------------
;; ghub (Forge's HTTP layer) searches auth-source for :host "api.github.com"
;; and :user "<github.user>^forge". We answer with the token from `gh auth
;; token', as a :secret closure like the netrc backend does.
(require 'auth-source)
(cl-defun dl/gh-cli-auth-source-search (&rest _spec &key host user &allow-other-keys)
  "Return the `gh auth token' as an auth-source entry for api.github.com."
  (when (member "api.github.com" (ensure-list host))
    (let ((token (string-trim (shell-command-to-string "gh auth token 2>/dev/null"))))
      (unless (string-empty-p token)
        ;; `apply-partially' rather than a lambda: the tangled config.el
        ;; is loaded without lexical-binding, so a lambda can't capture TOKEN.
        (list (list :host "api.github.com"
                    :user (car (ensure-list user))
                    :secret (apply-partially #'identity token)))))))

(defun dl/gh-cli-auth-source-backend-parse (entry)
  "Turn the symbol `gh-cli' in `auth-sources' into a backend."
  (when (eq entry 'gh-cli)
    (auth-source-backend
     :source "gh-cli"
     :type 'gh-cli
     :search-function #'dl/gh-cli-auth-source-search)))

(add-hook 'auth-source-backend-parser-functions #'dl/gh-cli-auth-source-backend-parse)
(add-to-list 'auth-sources 'gh-cli t)

;; --- Forge itself ----------------------------------------------------------
;; evil-collection installs its own Forge bindings (`@' for the menu and the
;; extra entries in Magit's transient menus). Tell Forge not to add the
;; default set too, or the menus end up with duplicate entries. Must be nil
;; before Forge loads, which `:after magit' guarantees happens after this.
(setq forge-add-default-bindings nil)

;; Forge's buffers derive from `magit-mode', so they inherit the evil normal
;; initial state set in the Magit block above.
(use-package forge
  :after magit)

;; --- PR review flow --------------------------------------------------------
;; Magit's `d d' (diff dwim) already shows a pull request's whole diff when
;; point is on a PR line in the status buffer, but inside the PR buffer it
;; only works while point is on the "Commits" heading. This shows the whole
;; diff from anywhere in the PR buffer: the PR's head against its merge-base
;; with the target branch, which is what GitHub's "Files changed" tab shows.
(defun dl/forge-pullreq-diff ()
  "Show the full diff of the current pull request in a Magit diff buffer."
  (interactive)
  (let* ((pullreq (forge-current-pullreq t))
         (range (forge--pullreq-range pullreq t)))
    (unless range
      (user-error "PR #%s has not been fetched yet; run `@ f f' first"
                  (oref pullreq number)))
    (magit-diff-range range (magit-diff-arguments))))

(with-eval-after-load 'forge
  ;; `forge-pullreq-mode-map' inherits Magit's evil "overriding" marker, so
  ;; a plain define-key beats evil's own `d' (delete) in PR buffers. Don't
  ;; use an evil auxiliary map here: one on this child keymap would shadow
  ;; the evil-collection Magit bindings (j/k/…) inherited from magit-mode-map.
  (define-key forge-pullreq-mode-map (kbd "d") #'dl/forge-pullreq-diff)

  ;; evil-collection moves Magit's `j' jump menu to `g' prefixes (gs, gu, …)
  ;; but never adds Forge's two jumps, so `j N p' from the docs does nothing
  ;; here. Follow the same pattern.
  (evil-collection-define-key 'normal 'magit-status-mode-map
    "gP" #'forge-jump-to-pullreqs
    "gI" #'forge-jump-to-issues))

;; Open a PR by number/title from anywhere in the project, no status buffer
;; needed. Then `d' for the diff, `C-c RET' for the actions menu.
(dl/leader-keys
  "gp"  '(forge-visit-pullreq :which-key "open pull request"))

(require 'url)
(require 'json)

;; Let auth-source read the desktop keyring (KWallet's Secret Service) in
;; addition to the authinfo files, so the Linear key can live there.
(add-to-list 'auth-sources 'default t)

(defcustom dl/magit-linear-cache-ttl 300
  "Seconds to reuse a Linear result before asking the API again."
  :type 'integer :group 'magit)

(defcustom dl/magit-linear-limit 10
  "How many open Linear issues the list shows. 0 hides the list."
  :type 'integer :group 'magit)

(defcustom dl/magit-linear-views
  '(("mine" . "assignee:{isMe:{eq:true}}")
    ("team" . "team:{key:{eq:\"CON\"}}")
    ("all"  . "")
    ("bugs" . "labels:{some:{name:{eqIgnoreCase:\"bug\"}}}"))
  "Named views: NAME -> a GraphQL IssueFilter fragment (\"\" = no filter).
Besides these, `dl/magit-linear-set-view' accepts label:NAME, project:NAME,
assignee:NAME and team:KEY."
  :type '(alist :key-type string :value-type string) :group 'magit)

(defvar dl/magit-linear-view "mine"
  "Name of the view currently shown; see `dl/magit-linear-views'.")

(defun dl/magit-linear--gql-string (s)
  "Quote S as a GraphQL string literal."
  (concat "\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" s) "\""))

(defun dl/magit-linear--view-filter (view)
  "Return the IssueFilter fragment for VIEW, a preset name or KIND:VALUE."
  (or (cdr (assoc view dl/magit-linear-views))
      (pcase (split-string view ":" nil "[ \t]+")
        (`("label" ,name)    (format "labels:{some:{name:{eqIgnoreCase:%s}}}"
                                     (dl/magit-linear--gql-string name)))
        (`("project" ,name)  (format "project:{name:{containsIgnoreCase:%s}}"
                                     (dl/magit-linear--gql-string name)))
        (`("assignee" ,name) (format "assignee:{displayName:{containsIgnoreCase:%s}}"
                                     (dl/magit-linear--gql-string name)))
        (`("team" ,key)      (format "team:{key:{eq:%s}}"
                                     (dl/magit-linear--gql-string (upcase key))))
        ;; A bare word is taken as a label name.
        (`(,name)            (format "labels:{some:{name:{eqIgnoreCase:%s}}}"
                                     (dl/magit-linear--gql-string name)))
        (_ (user-error "Unknown Linear view: %s" view)))))

(defcustom dl/magit-linear-remote-regexp "github\\.com[:/]conductly/"
  "Only show the Linear section in repos whose origin URL matches this."
  :type 'regexp :group 'magit)

(defvar dl/magit-linear--cache (make-hash-table :test #'equal)
  "Cache keyed by \"VIEW|IDENTIFIER\": (FETCHED-AT . RESULT).
RESULT is (:branch ISSUE :list ISSUES) or (:error STRING).")

(defvar dl/magit-linear--inflight (make-hash-table :test #'equal)
  "Keys with a request in progress -> the Magit buffer that asked.")

(defun dl/magit-linear--api-key ()
  "Return the Linear API key from auth-source, or nil."
  (when-let* ((entry (car (auth-source-search :host "api.linear.app" :max 1)))
              (secret (plist-get entry :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun dl/magit-linear--branch-issue ()
  "Return (TEAM . NUMBER) named by the current branch, or nil.
Matches a 2-5 letter team key and a number at the start of the branch
name, optionally after a prefix/: con-1513-foo, CON1513, feature/con-1513."
  (when-let* ((branch (magit-get-current-branch)))
    (when (string-match "\\(?:\\`\\|/\\)\\([[:alpha:]]\\{2,5\\}\\)-?\\([0-9]+\\)\\(?:[^0-9]\\|\\'\\)" branch)
      (cons (upcase (match-string 1 branch))
            (string-to-number (match-string 2 branch))))))

(defun dl/magit-linear--query (with-branch view)
  "Build the GraphQL query for VIEW.
WITH-BRANCH adds the lookup by team key + number."
  (let ((view-filter (dl/magit-linear--view-filter view)))
    (concat
     (if with-branch "query($team:String!,$number:Float!,$limit:Int!){" "query($limit:Int!){")
     (when with-branch
       "branch: issues(first:1, filter:{team:{key:{eq:$team}}, number:{eq:$number}}){nodes{...F}} ")
     "list: issues(first:$limit, orderBy:updatedAt, filter:{"
     (unless (string-empty-p view-filter) (concat view-filter ", "))
     "state:{type:{nin:[\"completed\",\"canceled\",\"duplicate\"]}}}){nodes{...F}}}"
     " fragment F on Issue{identifier title url priorityLabel state{name type} assignee{displayName}}")))

(defun dl/magit-linear--fetch (key ref view api-key buffer)
  "Start an async request for cache KEY; REF is (TEAM . NUMBER) or nil.
The reply is handled by `dl/magit-linear--callback' and redraws BUFFER."
  (puthash key buffer dl/magit-linear--inflight)
  (let* ((vars (append (and ref `(:team ,(car ref) :number ,(cdr ref)))
                       `(:limit ,dl/magit-linear-limit)))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,api-key)))
         (url-request-data (encode-coding-string
                            (json-serialize `(:query ,(dl/magit-linear--query ref view)
                                              :variables ,vars))
                            'utf-8)))
    (url-retrieve "https://api.linear.app/graphql"
                  #'dl/magit-linear--callback (list key) t t)))

(defun dl/magit-linear--callback (status key)
  "Parse the Linear reply for KEY, cache it, and redraw the asking buffer."
  (let* ((failure (plist-get status :error)) ; e.g. (error http 400) or (error connection-failed ...)
         (http-status (and (boundp 'url-http-response-status) url-http-response-status))
         (result
          (condition-case err
              (progn
                ;; Only a connection-level failure has no body worth reading;
                ;; an HTTP 4xx from Linear still carries the real message.
                (when (and failure (not (eq (cadr failure) 'http)))
                  (signal (car failure) (cdr failure)))
                (goto-char (or (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                               (point-min)))
                (let* ((body (decode-coding-string
                              (buffer-substring-no-properties (point) (point-max)) 'utf-8))
                       (json (json-parse-string body :object-type 'alist
                                                :array-type 'list :null-object nil))
                       (errors (alist-get 'errors json))
                       (data (alist-get 'data json)))
                  (cond (errors (list :error (alist-get 'message (car errors))))
                        (data (list :branch (car (alist-get 'nodes (alist-get 'branch data)))
                                    :list (alist-get 'nodes (alist-get 'list data))))
                        (t (list :error (format "HTTP %s" http-status))))))
            (error (list :error (if failure
                                    (format "HTTP %s" (or http-status (error-message-string err)))
                                  (error-message-string err))))))
         (buffer (gethash key dl/magit-linear--inflight)))
    (kill-buffer (current-buffer))
    (puthash key (cons (float-time) result) dl/magit-linear--cache)
    (remhash key dl/magit-linear--inflight)
    ;; Redraw from the command loop, not from inside the network callback.
    (when (buffer-live-p buffer)
      (run-at-time 0 nil #'dl/magit-linear--redraw buffer))))

(defun dl/magit-linear--redraw (buffer)
  (with-current-buffer buffer
    (when (derived-mode-p 'magit-status-mode)
      (magit-refresh-buffer))))

(defun dl/magit-linear--insert-issue (issue &optional note)
  "Insert one ISSUE line as a `linear-issue' section; NOTE trails it dimmed."
  (let* ((state (alist-get 'state issue))
         (type (alist-get 'type state))
         (state-face (pcase type
                       ("started" 'success)
                       ((or "completed" "canceled" "duplicate") 'magit-dimmed)
                       (_ 'default))))
    (magit-insert-section (linear-issue issue)
      (insert (propertize (alist-get 'identifier issue) 'face 'magit-hash)
              " "
              (propertize (format "%-12s" (alist-get 'name state)) 'face state-face)
              " "
              (alist-get 'title issue)
              (if note (propertize (concat "  " note) 'face 'magit-dimmed) "")
              "\n"))))

(defun dl/magit-insert-linear ()
  "Insert the Linear section into the Magit status buffer."
  (let* ((origin (magit-get "remote" "origin" "url"))
         (ref (dl/magit-linear--branch-issue)))
    (when (or ref (and origin (string-match-p dl/magit-linear-remote-regexp origin)))
      (let ((api-key (dl/magit-linear--api-key)))
        (if (not api-key)
            (magit-insert-section (linear)
              (magit-insert-heading
                "Linear " (propertize "(no API key in the keyring; see config.org)"
                                      'face 'magit-dimmed))
              (insert "\n"))
          (let* ((ident (and ref (format "%s-%d" (car ref) (cdr ref))))
                 (view dl/magit-linear-view)
                 (key (format "%s|%s" view (or ident "")))
                 (entry (gethash key dl/magit-linear--cache))
                 (fresh (and entry (< (- (float-time) (car entry)) dl/magit-linear-cache-ttl)))
                 (result (cdr entry)))
            (unless (or fresh (gethash key dl/magit-linear--inflight))
              (dl/magit-linear--fetch key ref view api-key (current-buffer)))
            (magit-insert-section (linear)
              (magit-insert-heading
                "Linear "
                (propertize (format "(%s%s)" view
                                    (cond ((null entry) ", fetching…")
                                          ((not fresh) ", refreshing…")
                                          (t "")))
                            'face 'magit-dimmed))
              (cond
               ((plist-get result :error)
                (insert (propertize (format "error: %s\n" (plist-get result :error))
                                    'face 'magit-dimmed)))
               (entry
                (let ((branch-issue (plist-get result :branch)))
                  (cond (branch-issue
                         (dl/magit-linear--insert-issue branch-issue "← this branch"))
                        (ident
                         (insert (propertize (format "%s  not found in Linear\n" ident)
                                             'face 'magit-dimmed))))
                  (dolist (issue (plist-get result :list))
                    (unless (equal (alist-get 'identifier issue) ident)
                      (dl/magit-linear--insert-issue issue))))))
              (insert "\n"))))))))

(defun dl/magit-linear-browse ()
  "Open the Linear issue at point in the browser."
  (interactive)
  (if-let* ((issue (magit-section-value-if 'linear-issue)))
      (browse-url (alist-get 'url issue))
    (user-error "No Linear issue at point")))

(defun dl/magit-linear-copy-url ()
  "Copy the URL of the Linear issue at point."
  (interactive)
  (if-let* ((issue (magit-section-value-if 'linear-issue)))
      (progn (kill-new (alist-get 'url issue))
             (message "Copied %s" (alist-get 'url issue)))
    (user-error "No Linear issue at point")))

(defun dl/magit-linear-refresh ()
  "Drop the Linear cache and refresh the status buffer."
  (interactive)
  (clrhash dl/magit-linear--cache)
  (magit-refresh))

(defun dl/magit-linear-set-view (view)
  "Switch the Linear list to VIEW and refetch it.
VIEW is a preset from `dl/magit-linear-views' or label:NAME, project:NAME,
assignee:NAME, team:KEY. Picking the current view again just refetches."
  (interactive
   (list (completing-read
          (format "Linear view (current: %s; or label:/project:/assignee:/team:NAME): "
                  dl/magit-linear-view)
          (mapcar #'car dl/magit-linear-views) nil nil nil nil dl/magit-linear-view)))
  (dl/magit-linear--view-filter view)   ; validate before switching
  (setq dl/magit-linear-view view)
  (maphash (lambda (k _) (when (string-prefix-p (concat view "|") k)
                           (remhash k dl/magit-linear--cache)))
           dl/magit-linear--cache)
  (magit-refresh))

;; Magit finds a section's keymap by name: magit-<type>-section-map.
(defvar-keymap magit-linear-issue-section-map
  "<remap> <magit-visit-thing>"  #'dl/magit-linear-browse
  "<remap> <magit-browse-thing>" #'dl/magit-linear-browse
  "<remap> <magit-copy-thing>"   #'dl/magit-linear-copy-url
  "RET"                          #'dl/magit-linear-browse)

(defvar-keymap magit-linear-section-map
  "<remap> <magit-visit-thing>" #'dl/magit-linear-set-view
  "RET"                         #'dl/magit-linear-set-view)

(with-eval-after-load 'magit
  (magit-add-section-hook 'magit-status-sections-hook
                          #'dl/magit-insert-linear
                          'magit-insert-status-headers t))

(use-package nix-mode
  :mode "\\.nix\\'")

;; This uses dockerfile-mode for Docker files
(use-package dockerfile-mode)
(put 'dockerfile-image-name 'safe-local-variable #'stringp)
(add-to-list 'auto-mode-alist '("\\Dockerfile?$" . dockerfile-mode)) ;; auto-enable for Dockerfiles

(use-package terraform-mode
  :hook ((terraform-mode . lsp-deferred)
         (terraform-mode . terraform-format-on-save-mode)))

(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))

;; Copilot configuration
;; Note: Since copilot.el is not in standard package repos,
;; you'll need to manually clone it:
;; git clone https://github.com/zerolfx/copilot.el ~/.emacs.d/copilot.el

(when (file-exists-p "~/.emacs.d/copilot.el")
  (add-to-list 'load-path "~/.emacs.d/copilot.el")
  (require 'copilot nil t)
  
  (add-hook 'prog-mode-hook 'copilot-mode)
  
  (with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)))

(defvar dl/prompts-directory "~/.local/share/prompts"
  "Directory containing LLM prompt files.")

(defun dl/get-prompt-files ()
  "Get list of prompt files from the prompts directory."
  (when (file-directory-p dl/prompts-directory)
    (directory-files dl/prompts-directory nil "\\.org$")))

(defun dl/read-prompt-file (filename)
  "Read the contents of a prompt file."
  (let ((filepath (expand-file-name filename dl/prompts-directory)))
    (when (file-exists-p filepath)
      (with-temp-buffer
        (insert-file-contents filepath)
        (buffer-string)))))

(defun dl/format-prompt-name (filename)
  "Format filename for display (remove .org extension)."
  (file-name-sans-extension filename))

(defun dl/llm-prompt-selector ()
  "Select a LLM prompt and copy it to clipboard."
  (interactive)
  (let ((prompt-files (dl/get-prompt-files)))
    (if prompt-files
        (ivy-read "Select LLM prompt: "
                  (mapcar #'dl/format-prompt-name prompt-files)
                  :action (lambda (prompt-name)
                            (let* ((filename (concat prompt-name ".org"))
                                   (content (dl/read-prompt-file filename)))
                              (if content
                                  (progn
                                    (kill-new content)
                                    (message "Prompt '%s' copied to clipboard!" prompt-name))
                                (message "Error: Could not read prompt file")))))
      (message "No prompt files found in %s" dl/prompts-directory))))

(defun dl/open-prompts-directory ()
  "Open the prompts directory in dired."
  (interactive)
  (if (file-directory-p dl/prompts-directory)
      (dired dl/prompts-directory)
    (progn
      (make-directory dl/prompts-directory t)
      (dired dl/prompts-directory)
      (message "Created prompts directory: %s" dl/prompts-directory))))

(defun dl/create-new-prompt ()
  "Create a new prompt file."
  (interactive)
  (unless (file-directory-p dl/prompts-directory)
    (make-directory dl/prompts-directory t))
  (let ((prompt-name (read-string "Prompt name: ")))
    (when (and prompt-name (not (string-empty-p prompt-name)))
      (let ((filename (expand-file-name
                       (concat prompt-name ".org")
                       dl/prompts-directory)))
        (find-file filename)
        (when (= (buffer-size) 0)
          (insert (format "#+TITLE: %s\n#+AUTHOR: %s\n#+DATE: %s\n\n"
                          prompt-name
                          user-full-name
                          (format-time-string "%Y-%m-%d")))
          (goto-char (point-max)))))))

;; Add leader key bindings for LLM prompts
(dl/leader-keys
  "i"   '(:ignore t :which-key "prompts")
  "is"  '(dl/llm-prompt-selector :which-key "select prompt")
  "io"  '(dl/open-prompts-directory :which-key "open prompts dir")
  "in"  '(dl/create-new-prompt :which-key "new prompt"))

;; Optional: Global keybinding for quick access
(global-set-key (kbd "C-c C-p") 'dl/llm-prompt-selector)

;; Claude Code integration
;; Note: Requires Claude Code CLI to be installed and available in PATH
;; Also requires transient package (0.7.5+)

;; Since claude-code.el is not in standard package repos,
;; we'll clone it similar to copilot.el
(when (executable-find "claude")
  ;; Clone claude-code.el if it doesn't exist
  (let ((claude-code-dir "~/.emacs.d/claude-code.el"))
    (unless (file-exists-p claude-code-dir)
      (message "Claude Code directory not found. Clone it with:")
      (message "git clone https://github.com/stevemolitor/claude-code.el ~/.emacs.d/claude-code.el"))
    
    ;; Load claude-code if directory exists
    (when (file-exists-p claude-code-dir)
      (add-to-list 'load-path claude-code-dir)
      (require 'claude-code nil t)
      
      ;; Enable claude-code-mode
      (claude-code-mode)
      
      ;; Set up claude-code keybindings
      ;; First unbind any existing 'c' binding, then set it as a prefix
      (dl/leader-keys
        "c" nil)  ; Explicitly unbind first
      
      (dl/leader-keys
        "c"  '(:ignore t :which-key "claude")
        "cc" '(claude-code :which-key "start claude")
        "cs" '(claude-code-send-command :which-key "send command")
        "cr" '(claude-code-send-region :which-key "send region")
        "cm" '(claude-code-transient :which-key "transient menu"))
      
      ;; Also bind the command map to C-c c for compatibility
      (define-key global-map (kbd "C-c c") claude-code-command-map)
      
      ;; Optional: Configure claude-code settings
      (setq claude-code-terminal-backend 'eat)  ; or 'vterm' if you prefer
      (setq claude-code-desktop-notifications t))))

(defvar dl/org-files-directory "~/org"
  "Directory containing personal org files.")

(defun dl/open-org-file (filename)
  "Open an org file from the org directory."
  (interactive)
  (let ((filepath (expand-file-name filename dl/org-files-directory)))
    (if (file-exists-p filepath)
        (find-file filepath)
      (message "File not found: %s" filepath))))

(defun dl/get-org-files ()
  "Get list of org files from the org directory."
  (when (file-directory-p dl/org-files-directory)
    (directory-files dl/org-files-directory nil "\\.org$")))

(defun dl/org-file-selector ()
  "Select an org file from your org directory to open."
  (interactive)
  (let ((org-files (dl/get-org-files)))
    (if org-files
        (ivy-read "Select org file: "
                  org-files
                  :action (lambda (filename)
                            (dl/open-org-file filename)))
      (message "No org files found in %s" dl/org-files-directory))))

;; Add leader key bindings for org files
(dl/leader-keys
  "f"   '(:ignore t :which-key "org files")
  "ff"  '(dl/org-file-selector :which-key "find org file")
  "fp"  '(dl/open-emacs-php-workflow :which-key "php workflow")
  "fo"  '((lambda () (interactive) (dired dl/org-files-directory)) :which-key "open org dir")
  "fr"  '(recentf-open-files :which-key "recent files"))

(use-package which-key
  :ensure nil  ; Managed by Nix
  :init
  (setq which-key-idle-delay 0.3
        which-key-idle-secondary-delay 0.1)
  :config
  (which-key-mode))

(use-package helpful
  :ensure nil  ; Managed by Nix
  :commands (helpful-callable helpful-variable helpful-key)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command]  . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(with-eval-after-load 'org
  (org-babel-do-load-languages
  'org-babel-load-languages
  '(
    (emacs-lisp . t)
    (python . t)
    (sql . t)
    (shell . t)))
 )

(defun dl/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'dl/babel-ansi)
