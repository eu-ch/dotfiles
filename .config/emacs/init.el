;; ------------------------------------------
;; Helper functions and variables

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

(desktop-save-mode 1)

; ------------------------------------------
; Packages
; ------------------------------------------

(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq enable-recursive-minibuffers t)
  :custom
  (inhibit-eol-conversion t)
  :hook
  (minibuffer-setup . cursor-intangible-mode))

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
(use-package doom-themes)
(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-height 30)
  (set-face-attribute 'mode-line nil :height 140)
  (doom-modeline-mode 1))
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
  :hook
  (progn-mode . global-tree-sitter-mode)
  (prog-mode . tree-sitter-hl-mode))
(use-package tree-sitter-langs :after tree-sitter)
(use-package dired
  :ensure nil
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
  ;; (define-key evil-normal-state-map (kbd "SPC B") 'consult-project-buffer)
  (define-key evil-normal-state-map (kbd "SPC e") 'consult-recent-file)
  (define-key evil-normal-state-map (kbd "SPC j") 'dired-jump)
  (define-key evil-normal-state-map (kbd "SPC m") 'consult-bookmark)
  (define-key evil-normal-state-map (kbd "SPC M") 'bookmark-set)
  (define-key evil-normal-state-map (kbd "SPC N") 'make-frame)
  (define-key evil-normal-state-map (kbd "SPC o") 'ff-find-other-file)
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
  (setq treemacs-no-png-images t))
(use-package treemacs-evil :after treemacs)
(use-package zig-mode)

(setq-default buffer-file-coding-system 'utf-8-unix)

;; -----------------------------------------
;; AUTO CONFIG
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-gruvbox))
 '(custom-safe-themes
   '("d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69" "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a" "7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9" "e14884c30d875c64f6a9cdd68fe87ef94385550cab4890182197b95d53a7cf40" "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b" "dd4582661a1c6b865a33b89312c97a13a3885dc95992e2e5fc57456b4c545176" "0c83e0b50946e39e237769ad368a08f2cd1c854ccbcd1a01d39fdce4d6f86478" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "7ea883b13485f175d3075c72fceab701b5bf76b2076f024da50dff4107d0db25" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "ae426fc51c58ade49774264c17e666ea7f681d8cae62570630539be3d06fd964" "636b135e4b7c86ac41375da39ade929e2bd6439de8901f53f88fde7dd5ac3561" "f64189544da6f16bab285747d04a92bd57c7e7813d8c24c30f382f087d460a33" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "4fda8201465755b403a33e385cf0f75eeec31ca8893199266a6aeccb4adedfa4" "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851" "3fe1ebb870cc8a28e69763dde7b08c0f6b7e71cc310ffc3394622e5df6e4f0da" "be84a2e5c70f991051d4aaf0f049fa11c172e5d784727e0b525565bb1533ec78" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "dc8285f7f4d86c0aebf1ea4b448842a6868553eded6f71d1de52f3dcbc960039" "5586a5db9dadef93b6b6e72720205a4fa92fd60e4ccfd3a5fa389782eab2371b" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "467dc6fdebcf92f4d3e2a2016145ba15841987c71fbe675dcfe34ac47ffb9195" "4ff1c4d05adad3de88da16bd2e857f8374f26f9063b2d77d38d14686e3868d8d" "f458b92de1f6cf0bdda6bce23433877e94816c3364b821eb4ea9852112f5d7dc" "016f665c0dd5f76f8404124482a0b13a573d17e92ff4eb36a66b409f4d1da410" "49acd691c89118c0768c4fb9a333af33e3d2dca48e6f79787478757071d64e68" "eca44f32ae038d7a50ce9c00693b8986f4ab625d5f2b4485e20f22c47f2634ae" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "2dd4951e967990396142ec54d376cced3f135810b2b69920e77103e0bcedfba9" default))
 '(package-selected-packages
   '(org-roam consult marginalia orderless vertico zig-mode treemacs-evil treemacs evil-nerd-commenter evil-collection evil dired-single doom-themes diminish which-key undo-fu tree-sitter-langs markdown-mode lua-mode editorconfig doom-modeline company))
 '(treemacs-width 40)
 '(warning-suppress-log-types '((perspective) (auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
