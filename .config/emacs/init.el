;; ------------------------------------------
; Helper functions and variables

; From https://blog.jmthornton.net/p/emacs-project-override
(defun project-root-override (dir)
  (let ((root (locate-dominating-file dir ".project.el"))
        (backend (ignore-errors (vc-responsible-backend dir))))
    (when root (if (version<= emacs-version "28")
                    (cons 'vc root)
                 (list 'vc backend root)))))
(defun ech/save-and-kill-this-buffer ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))

(defun ech/save-all ()
  (interactive)
  (save-some-buffers t))

(setq ech/init-dir (file-name-parent-directory user-init-file))

(defun eglot--clangd-switch-source-header ()
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

;; ------------------------------------------
;; UI

;; Scrolling behaviour
(setq-default redisplay-dont-pause nil
              scroll-conservatively 10000
              scroll-down-aggressively 0.01
              scroll-margin 5
              scroll-preserve-screen-position 1
              scroll-step 1
              scroll-up-aggressively 0.01
              truncate-lines nil)

;; Replace yes/no in prompts with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Other UI settings
(setq default-frame-alist '((width . 130)))
(set-face-attribute 'default nil :family "Source Code Pro" :height 115)

(column-number-mode 1)
(global-display-line-numbers-mode 1)
(recentf-mode)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; ------------------------------------------
;; Behaviour

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

;: Editing
(setq-default c-basic-offset 4
              indent-tabs-mode nil
              indicate-buffer-boundaries 'left
              indicate-empty-lines t
              show-trailing-whitespace nil
              tab-width 4)

(setq ff-other-file-alist
      '(("\\.c" (".h"))
        ("\\.h" (".cpp" ".c"))
        ("\\.cpp" (".h" ".hpp"))
        ("\\.hpp" (".cpp")))
      ff-search-directories
      '("." "../src" "../include" "../../src/*" "../../include/*" "../../src/*/*" "../../include/*/*")
      ff-always-try-to-create nil)

(desktop-save-mode 1)

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
  ;; :custom
  ;; (inhibit-eol-conversion t)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :config
  (setq custom-file (concat user-emacs-directory "ignored-custom-file.el"))
  (setq ring-bell-function 'ignore))

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
(use-package project
  :config
  (add-hook 'project-find-functions #'project-root-override))

(use-package org)
(use-package org-roam :after org)
(use-package eldoc :diminish :ensure nil)
(use-package nerd-icons)
(use-package diminish)
(use-package doom-themes
  :after doom-modeline centaur-tabs
  :config
  (setq custom-safe-themes t)
  (load-theme 'doom-opera))
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 30)
  :hook
  (after-init . doom-modeline-mode))
(use-package undo-fu)
(use-package which-key :diminish :config (which-key-mode))
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
(use-package editorconfig
  :diminish
  :config (editorconfig-mode 1))
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((c++-mode c-mode) "clangd" "--enable-config" "--background-index")))
(use-package lua-mode
  :config
  (setq lua-indent-level 4))
(use-package markdown-mode)
(use-package tree-sitter
  :diminish
  :config
  (setq treesit-language-source-alist
        '((yaml "https://github.com/ikatyang/tree-sitter-yaml")))
	     )
(use-package tree-sitter-langs :after tree-sitter
  :hook
  (progn-mode . global-tree-sitter-mode)
  (prog-mode . tree-sitter-hl-mode)
  )
(use-package dired
  :ensure nil
  :straight nil
  :commands (dired)
  :custom ((dired-listing-switches "-agho --group-directories-first")
           (dired-dwim-target 1)))
(use-package dired-single :after dired)
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
  :config
  ; Code modification
  (define-key evil-insert-state-map (kbd "M-SPC") 'company-complete)
  (define-key evil-normal-state-map (kbd "SPC a") 'eglot-code-actions)
  (define-key evil-normal-state-map (kbd "SPC f") 'eglot-format)
  (define-key evil-normal-state-map (kbd "SPC q") 'eglot-code-action-quickfix)
  (define-key evil-normal-state-map (kbd "SPC r") 'eglot-rename)
  (define-key evil-visual-state-map (kbd "SPC f") 'eglot-format)

  ; Code navigation
  (define-key evil-normal-state-map (kbd "<f12>") 'consult-imenu)
  (define-key evil-normal-state-map (kbd "g b") 'xref-pop-marker-stack)
  (define-key evil-normal-state-map (kbd "g D") 'eglot-find-declaration)
  (define-key evil-normal-state-map (kbd "g r") 'xref-find-references)
  (define-key evil-normal-state-map (kbd "SPC b") 'consult-buffer)
  (define-key evil-normal-state-map (kbd "SPC B") 'consult-project-buffer)
  (define-key evil-normal-state-map (kbd "SPC e") 'consult-recent-file)
  (define-key evil-normal-state-map (kbd "SPC j") 'dired-jump)
  (define-key evil-normal-state-map (kbd "SPC m") 'consult-bookmark)
  (define-key evil-normal-state-map (kbd "SPC M") 'bookmark-set)
  (define-key evil-normal-state-map (kbd "SPC N") 'make-frame)
  (define-key evil-normal-state-map (kbd "SPC o") 'ff-find-other-file)
  ;; (define-key evil-normal-state-map (kbd "SPC o") 'eglot--clangd-switch-source-header)
  ;; (define-key evil-normal-state-map (kbd "SPC p 1") 'projectile-configure-project)
  ;; (define-key evil-normal-state-map (kbd "SPC p 2") 'projectile-compile-project)
  ;; (define-key evil-normal-state-map (kbd "SPC p 3") 'projectile-run-project)
  (define-key evil-normal-state-map (kbd "SPC s") 'save-buffer)
  (define-key evil-normal-state-map (kbd "SPC S") 'ech/save-all)
  (define-key evil-normal-state-map (kbd "SPC t") 'treemacs)
  (define-key evil-normal-state-map (kbd "SPC w") 'kill-current-buffer)

  ; Random stuff
  (fset 'project-prefix-map project-prefix-map) ; this allows project-prefix-map to work with SPC p
  (define-key evil-normal-state-map (kbd "SPC p") 'project-prefix-map)
  (define-key evil-normal-state-map (kbd "SPC !")
              (lambda() (interactive)(find-file (concat ech/init-dir "init.el"))))
  (define-key evil-normal-state-map (kbd "SPC SPC e") 'eglot)
  ; Make Ctrl-U work correctly
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
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
(use-package treemacs
  :config
  (setq treemacs-no-png-images nil))
(use-package centaur-tabs
  :demand
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
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :hook
  (term-mode . centaur-tabs-local-mode))
(use-package treemacs-evil :after treemacs)
(use-package zig-mode)
