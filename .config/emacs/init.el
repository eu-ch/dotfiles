;; ------------------------------------------
; Helper functions and variables

;; Better errors when --debug-init flag is present
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq ech/init-dir (file-name-parent-directory user-init-file))

(defun ech/save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))
(defun ech/save-all ()
  (interactive)
  (save-some-buffers t))
(defun ech/edit-init-file ()
  (interactive)
  (find-file (concat ech/init-dir "init.el")))
(defun ech/clangd-switch-source-header ()
  "Switch between the corresponding C/C++ source and header file.
Only works with clangd."
  (interactive)
  (let ((response
         (eglot--request
          (eglot--current-server-or-lose)
          :textDocument/switchSourceHeader (eglot--TextDocumentIdentifier))))
    (if response
        ;;        (funcall #'find-file (eglot-uri-to-path response))
        (find-file (eglot-uri-to-path response))
      (user-error "Could not find other file"))))

; ------------------------------------------
; Packages
; ------------------------------------------

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq enable-recursive-minibuffers t)

  :config
  (setq custom-file (concat user-emacs-directory "ignored-custom-file.el"))
  (setq ring-bell-function 'ignore)

  ;; Replace yes/no in prompts with y/n
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Scrolling behaviour
  (setq-default redisplay-dont-pause nil
                scroll-conservatively 10000
                scroll-down-aggressively 0.01
                scroll-margin 5
                scroll-preserve-screen-position 1
                scroll-step 1
                scroll-up-aggressively 0.01
                truncate-lines nil)

  ;; Other UI settings
  (setq default-frame-alist '((width . 130)))
  (set-face-attribute 'default nil :family "Source Code Pro" :height 115)

  ;; Editing
  (setq-default c-basic-offset 4
                indent-tabs-mode nil
                indicate-buffer-boundaries 'left
                indicate-empty-lines t
                show-trailing-whitespace nil
                tab-width 4)

  ;; Encoding
  (set-language-environment "UTF-8")
  (setq default-input-method nil)
  (set-selection-coding-system (if (eq system-type 'windows-nt) 'utf-16-le 'utf-8))

  ;; Backup files
  (setq delete-by-copying -1
        delete-old-versions t
        version-control t
        auto-save-default nil
        create-lockfiles nil
        make-backup-files nil
        backup-directory-alist
        `(("." . (concat user-emacs-directory "backups"))))


  (setq ff-other-file-alist
          '(("\\.c" (".h"))
            ("\\.h" (".cpp" ".c"))
            ("\\.cpp" (".h" ".hpp"))
            ("\\.hpp" (".cpp")))
          ff-search-directories
          '("." "../src" "../include" "../../src/*" "../../include/*" "../../src/*/*" "../../include/*/*")
          ff-always-try-to-create nil)

  ;; Default modes
  (column-number-mode 1)
  (desktop-save-mode 1)
  (global-display-line-numbers-mode 1)
  (recentf-mode)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  :hook
  (minibuffer-setup . cursor-intangible-mode))

;; ------------------------------------------
;; Quality of life packages

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired)
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-dwim-target 1)))
(use-package dired-single :after dired)
(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t))
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
(use-package savehist
  :config
  (savehist-mode))
(use-package marginalia
  :config
  (marginalia-mode))
(use-package consult
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<"))

(use-package diminish)
(use-package eldoc :diminish :ensure nil)
(use-package nerd-icons)
(use-package undo-fu)
(use-package which-key :diminish :config (which-key-mode))

;; ------------------------------------------
;; UI packages

(use-package doom-themes
  :after doom-modeline
  :config
  (setq custom-safe-themes t)
  (load-theme 'doom-opera))
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 30)
  :hook
  (after-init . doom-modeline-mode))
(use-package treemacs
  :config
  (setq treemacs-no-png-images nil))
(use-package centaur-tabs
  :init
  (setq centaur-tabs-set-icons t
        centaur-tabs-gray-out-icons 'buffer
        centaur-tabs-set-bar 'left
        centaur-tabs-set-modified-marker t
        centaur-tabs-close-button "✕"
        centaur-tabs-modified-marker "•"
        centaur-tabs-icon-type 'nerd-icons
        centaur-tabs-style "rounded"
        ;; Scrolling (with the mouse wheel) past the end of the tab list
        ;; replaces the tab list with that of another Doom workspace. This
        ;; prevents that.
        centaur-tabs-cycle-scope 'tabs)
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'centaur-tabs-mode)
    (add-hook 'after-init #'centaur-tabs-mode))
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :hook
  (term-mode . centaur-tabs-local-mode))

;; ------------------------------------------
;; Development environment

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-auto-discover nil))
(use-package consult-projectile)
(use-package treemacs-projectile)

(use-package lsp-mode
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-clients-clangd-args
   '("-j=4"
     "--header-insertion=never"
     "--all-scopes-completion"
     "--background-index"
     "--cross-file-rename"
     "--suggest-missing-includes")))
(use-package lsp-ui :commands lsp-ui-mode)

(use-package company
  :diminish
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.1
        company-selection-wrap-around t
        company-tooltip-align-annotations t
        company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                            company-echo-metadata-frontend))
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))
(use-package tree-sitter
  :diminish
  :config
  (setq treesit-language-source-alist
        '((yaml "https://github.com/ikatyang/tree-sitter-yaml"))))
(use-package tree-sitter-langs :after tree-sitter
  :hook
  (progn-mode . global-tree-sitter-mode)
  (prog-mode . tree-sitter-hl-mode))

(use-package lua-mode :config (setq lua-indent-level 4))
(use-package markdown-mode)
(use-package zig-mode)

;; ------------------------------------------
;; EViL

(use-package evil
  :init
  (setq evil-overriding-maps '()
        evil-search-module 'evil-search
        evil-split-window-below t
        evil-undo-system 'undo-fu
        evil-vsplit-window-right t
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil)
  :hook
  (after-init . evil-mode)
  :bind
  (:map evil-normal-state-map
        ;; Code modification
        ("SPC a" . lsp-execute-code-action)
        ("SPC f" . lsp-format-buffer)
        ("SPC r" . lsp-rename)
        ;; Code navigation
        (  "g D" . lsp-find-definition)
        (  "g b" . xref-pop-marker-stack)
        (  "g r" . lsp-find-references)
        ("SPC B" . consult-projectile)
        ("SPC M" . bookmark-set)
        ("SPC N" . make-frame)
        ("SPC b" . consult-buffer)
        ("SPC e" . consult-recent-file)
        ("SPC j" . dired-jump)
        ("SPC l" . lsp-keymap-prefix)
        ("SPC m" . consult-bookmark)
        ("SPC o" . projectile-find-other-file)
        ("SPC s" . consult-imenu)
        ;; Buffers and windows
        ("SPC !" . ech/edit-init-file)
        ("SPC W" . ech/save-all)
        ("SPC p" . projectile-command-map)
        ("SPC q" . kill-current-buffer)
        ("SPC t" . treemacs)
        ("SPC w" . save-buffer)
        ;; Make Ctrl-U work correctly
        ( "C-u" . evil-scroll-up)
        )
  (:map evil-insert-state-map
        ;; Code modification
        ("M-." . company-complete)
        )
  (:map evil-visual-state-map
        ;; Code modification
        ("SPC f" . lsp-format-region)
        ;; Make Ctrl-U work correctly
        (" C-u" . evil-scroll-up)
        )
  :config
  ; Apply :q and :wq to a current buffer but not to a frame
  (evil-ex-define-cmd "q" #'kill-this-buffer)
  (evil-ex-define-cmd "wq" #'ech/save-and-kill-this-buffer))
(use-package evil-collection
  :diminish
  :after evil
  :config
  (evil-collection-init)
  (evil-collection-define-key
   'normal 'dired-mode-map
   "h" 'dired-single-up-directory
   "l" 'dired-single-buffer)
  (diminish 'evil-collection-unimpaired-mode))
(use-package evil-nerd-commenter
  :bind (("C-/" . evilnc-comment-or-uncomment-lines)))
(use-package treemacs-evil :after treemacs)
