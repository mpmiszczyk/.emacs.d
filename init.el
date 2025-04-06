(setq straight-bootstrap-use-nongnu-elpa nil)

(setq straight-recipe-overrides
      '((nil (nongnu-elpa :type git
                          :repo "https://github.com/emacsmirror/nongnu_elpa.git"
                          :depth (full single-branch)
                          :local-repo "nongnu-elpa"
                          :build nil))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (setq package-enable-at-startup nil)
;; (straight-use-package 'use-package)
;; (setq straight-use-package-by-default t)
;; (setq straight-allow-recipe-inheritance t)

(use-package straight
  :custom
  ;; add project and flymake to the pseudo-packages variable so straight.el doesn't download a separate version than what eglot downloads.
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))
  (straight-use-package-by-default t)
  (straight-allow-recipe-inheritance t)
  (package-enable-at-startup nil)
  (straight-bootstrap-use-nongnu-elpa nil)
  )

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(use-package org
  :bind (("C-c t n" . org-toggle-narrow-to-subtree)
         ("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-'" . avy-goto-char-timer))

  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-done t)
  (require 'org-tempo)
  (require 'org-protocol)
  
  (setq org-capture-templates
        `(
	        ("p" "Protocol" entry (file+headline "~/roam/20231119151635-capture.org" "Inbox")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	        ("L" "Protocol Link" entry (file+headline "~/roam/20231119151635-capture.org" "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ))

  (add-hook 'org-mode-hook
            '(lambda ()
               (define-key org-mode-map [(control tab)] nil)
               (auto-fill-mode)
               (org-indent-mode)
               (rainbow-delimiters-mode-disable)
               (add-hook 'kill-emacs-hook 'org-clock-out-on-exit)))


  (defun org-clock-out-on-exit ()
    "Closes active clock when exiting emacs;


Still needs some work done; what important,
is saving all AFTER  clocking out.

I still need some functionality to handle
not eat defined org-clock-out function; which
is loaded dynamiclly"
    (org-clock-out nil t)
    (save-some-buffers))

  (setq org-clock-into-drawer t)

  (defun set-org-agenda-files ()
    (interactive)
    (setq org-agenda-files
          (mapcar 'abbreviate-file-name
                  (split-string
                   (shell-command-to-string
                    "find ~/Dropbox/org -name \"*.org\" -o -name \"*.org_archive\""  ) "\n"))))

  ;; Targets include this file and any file contributing to the agenda
  ;; - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 2)
                                   (org-agenda-files :maxlevel . 9))))

  ;; Use full outline paths for refile targets - we file directly with
  ;; IDO
  (setq org-refile-use-outline-path t)

  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)

  (setq org-refile-allow-creating-parent-nodes nil)

  ;; normal yank will act as smart one in org-mode
  (setq org-yank-adjusted-subtrees t))

(use-package org-download
  :after org-roam
  :custom
  (org-download-method 'attach)
  ;; (org-download-image-dir (expand-file-name "images" org-roam-directory))
  (org-download-screenshot-method "flameshot gui -p %s")
  )

(use-package org-journal
  :after (org)
  :custom
  (org-journal-dir "~/Dropbox/org/journal/")
  (org-journal-tag-alist '(("personal" . ?p)
                           ("dreams" . ?D)
                           ("memories" . ?m)
                           ("summary" . ?s)
                           ("writing" . ?W)
                           ("craft" . ?c)
                           ("work" . ?w)
                           ("daily" . ?d)
                           ("fresha" . ?f))))

(use-package elixir-ts-mode)

(use-package el-patch
  :straight (el-patch :type git :host github :repo "radian-software/el-patch"))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package treesit
  :straight (:type built-in)
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")
               (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
               (heex "https://github.com/phoenixframework/tree-sitter-heex")
               (nix "https://github.com/nix-community/tree-sitter-nix")
               ))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (json-mode . json-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)
                     (elixir-mode . elixir-ts-mode)
                     (nix-mode . nix-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  )

(use-package ts-movement
  :straight (ts-movement :type git :host github :repo "haritkapadia/ts-movement"))

(use-package combobulate
  :after (treesitter)
  :straight (combobulate :type git :host github :repo "mickeynp/combobulate")
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")

  ;; Optional, but recommended.
  ;;
  ;; You can manually enable Combobulate with `M-x
  ;; combobulate-mode'.
  :hook ((python-ts-mode . combobulate-mode)
         (js-ts-mode . combobulate-mode)
         (css-ts-mode . combobulate-mode)
         (yaml-ts-mode . combobulate-mode)
         (json-ts-mode . combobulate-mode)
         (typescript-ts-mode . combobulate-mode)
         (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  )

(setq custom-file (make-temp-file "emacs-custom"))
(load custom-file)

(use-package emacs
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  :custom
  (default-frame-alist '((font . "Iosevka Term Nerd Font-13")
                         (font . "Iosevka Nerd Font-12")
                         (font . "Inconsolata Nerd Font-g-11")))

  ;; scroll one line at a time (less "jumpy" than defaults)
  (mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
  (mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (mouse-wheel-follow-mouse nil) ;; scroll window under mouse
  (scroll-step 1) ;; keyboard scroll one line at a time
  (inhibit-startup-message t)
  (visible-bell t)
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "google-chrome-stable")
  (global-subword-mode t)
  ;; (set-face-attribute 'default nil :font "Inconsolata Nerd Font-g-12")
  ;; (set-face-attribute 'default nil :font "Inconsolata Nerd Font-14")
  ;; (set-face-attribute 'default nil :font "Iosevka Term Nerd Font-14")
  ;; (set-face-attribute 'default nil :font "Iosevka Nerd Font-13")
  ;; (set-face-attribute 'default nil :font "Iosevka Term-12")
  ;; (set-face-attribute 'default nil :font "Iosevka Term-13")
  ;; (set-face-attribute 'default nil :font "Iosevka Term-14")
  ;; (set-face-attribute 'default nil :font "Iosevka Term-16")
 )

(use-package string-inflection
  :bind
  (("C-c C-u" . string-inflection-lower-camelcase)
   ("C-c C-l" . string-inflection-underscore))
  )

(use-package term
  :config
  (term-set-escape-char ?\C-x)
  )

(use-package vterm
  :straight '(vterm :no-clone t) ;; installed trough home-manager
  :config
  (setq vterm-always-compile-module t)
  )

(use-package with-editor
  :after (vterm)
  :hook
  ((shell-mode
    eshell-mode
    term-mode
    vterm-mode) . with-editor-export-editor)
  )

;; ;; TODO fixes: var used way before it is introduced.  It seems it is
;; ;; used in `project.el', but it needs to be defined way sooner.
;; ;; Remove at some point in future.
;; (unless (boundp 'tab-prefix-map)
;;   (defvar tab-prefix-map (make-sparse-keymap)))

;; TODO should be moved somewhere sensible
(use-package ansi-color ; library needed to be loaded for `ansi-color-apply-on-region'
  :config
  (defun use-ansi-colors ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

;; (use-package golden-ratio
;;   :config
;;   (golden-ratio-mode nil)
;;   (setq golden-ratio-auto-scale nil
;;         golden-ratio-adjust-factor 0.2
;;         golden-ratio-wide-adjust-factor 0.2)
;;   )

(use-package zoom
  :config
  (zoom-mode t)
  :custom
  (zoom-size '(105 . 20))
  (zoom-ignored-major-modes '(dired-mode markdown-mode ediff-mode))
  (zoom-ignored-buffer-names '("*Ediff Control Panel*"))
  :bind
  (("C-x C-z" . zoom-mode))
  )

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :custom
  (copilot-max-char 500000)
  (copilot-indent-offset-warning-disable t)
  (qcopilot-idle-delay 5)
  :hook ((prog-mode) . copilot-mode)
  :bind (("C-o" . copilot-complete)
         ("C-M-o" . copilot-accept-completion)
         ("M-o" . copilot-accept-completion-by-word)
         ("M-S-o" . copilot-next-completion)
         ("C-S-O" . copilot-previous-completion)
         )
  )

(use-package codeium
    ;; if you use straight
    :straight '(:type git :host github :repo "Exafunction/codeium.el")
    ;; otherwise, make sure that the codeium.el file is on load-path

    :init
    ;; use globally
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
    ;; or on a hook
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions '(codeium-completion-at-point))))

    ;; if you want multiple completion backends, use cape (https://github.com/minad/cape):
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super #'codeium-completion-at-point #'lsp-completion-at-point)))))
    ;; an async company-backend is coming soon!

    ;; codeium-completion-at-point is autoloaded, but you can
    ;; optionally set a timer, which might speed up things as the
    ;; codeium local language server takes ~0.2s to start up
    ;; (add-hook 'emacs-startup-hook
    ;;  (lambda () (run-with-timer 0.1 nil #'codeium-init)))

    ;; :defer t ;; lazy loading, if you want
    (setq use-dialog-box nil);; do not use popup boxes

    ;; get codeium status in the modeline
    (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
    (add-to-list 'mode-line-format '(:eval (car-safe codeium-mode-line)) t)
    ;; alternatively for a more extensive mode-line
    ;; (add-to-list 'mode-line-format '(-50 "" codeium-mode-line) t)

    ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
    (setq codeium-api-enabled
        (lambda (api)
            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
    ;; you can also set a config for a single buffer like this:
    ;; (add-hook 'python-mode-hook
    ;;     (lambda ()
    ;;         (setq-local codeium/editor_options/tab_size 4)))

    ;; You can overwrite all the codeium configs!
    ;; for example, we recommend limiting the string sent to codeium for better performance
    ;; (defun my-codeium/document/text ()
    ;;     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
    ;; ;; if you change the text, you should also change the cursor_offset
    ;; ;; warning: this is measured by UTF-8 encoded bytes
    ;; (defun my-codeium/document/cursor_offset ()
    ;;     (codeium-utf8-byte-length
    ;;         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
    ;; (setq codeium/document/text 'my-codeium/document/text)
    ;; (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset)
    )

(use-package shell-maker)

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode shell-maker)
  :config
  (copilot-chat-backend 'curl)
  (copilot-chat-frontend 'shell-maker)
  )


(use-package gptel
  :init
  (defun gptel-apikey-from-auth-source (host user)
    "Return a function that retrieves the API key for the given HOST and USER."
    (let ((entry (car (auth-source-search :host host :user user))))
      (when entry
        (funcall (plist-get entry :secret))))
    )

  :custom
  (gptel-default-mode 'markdown-mode)
  :config
  (setq  gptel-backend
         (gptel-make-gemini "Gemini"
                 :key (gptel-apikey-from-auth-source "generativelanguage.googleapis.com" "apikey")
                 :stream t))
  ;; (setq gptel-backend
  ;;       (gptel-make-anthropic "Claude"
  ;;         :key (gptel-apikey-from-auth-source "api.anthropic.com" "apikey")
  ;;         :stream t
  ;;         )
  ;;       )
  )


;; (use-package nord-theme)
;; (use-package zenburn-theme)
;; (use-package nano-theme
;;   ;; :config
;;   ;; (load-theme 'nano-dark 'no-confirm)
;;   )
;; (use-package cybercafe-theme
;;   ;; :config
;;   ;; (load-theme 'cybercafe 'no-confirm)
;;   )
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night 'no-confirm)
  )

;; (use-package modus-themes
;;   ;; :config
;;   ;; (if (daemonp)
;;   ;;     (add-hook 'after-make-frame-functions
;;   ;;               (lambda (frame)
;;   ;;                 (with-selected-frame frame
;;   ;;                   (load-theme 'nono 'no-confirm))))
;;   ;;   (load-theme 'modus-vivendi 'no-confirm))
;;   )


;; note to myself:
;; ediff docs suck
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-to-list 'load-path "~/.emacs.d/lisp")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-copy-env "GUIX_PROFILE")
  (exec-path-from-shell-copy-env "KERL_CONFIGURE_OPTIONS")
  (exec-path-from-shell-initialize))

(use-package envrc
  :straight '(envrc :no-clone t) ;; installed trough home-manager
  :hook (after-init . envrc-global-mode))

;; (use-package inheritenv
;;   :straight (inheritenv :type git :host github :repo "purcell/inheritenv"))
;;  :hook (prog-mode . inheritenv-apply))

(use-package peep-dired
  :after dired
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package ag
  :custom
  (ag-highlight-search t)
  (ag-reuse-buffers t))

(use-package auto-highlight-symbol)

(use-package markdown-mode)

(use-package erlang
  :straight '(erlang :no-clone t) ;; installed trough home-manager
  :config
  (setq erlang-indent-level 2))

(use-package elixir-mode)

(use-package ruby-mode
  :custom
  (ruby-insert-encoding-magic-comment nil)
  :hook
  (before-save . eglot-format-buffer)
  )

(use-package jest-test-mode
  :commands jest-test-mode
  :hook
  (typescript-mode typescript-ts-mode js-ts-mode typescript-tsx-mode)
  )

(use-package flymake
  :bind
  (("M-n" . flymake-goto-next-error)
   ("M-p" . flymake-goto-prev-error)
   ("C-x M-d" . flymake-show-project-diagnostics))
  )


;; (use-package eglot-grammarly
;;   :straight (:host github :repo "emacs-grammarly/eglot-grammarly")
;;   :after eglot
;;   :defer t  ; defer package loading
;;   :hook ((text-mode markdown-mode). (lambda ()
;;                                       (require 'eglot-grammarly)
;;                                       (eglot-ensure))))

(use-package eglot
  :straight '(eglot :no-clone t) ;; installed trough home-manager
  :after (projectile project)
  :commands (eglot eglot-ensures)
  :hook ((elixir-mode
          elixir-ts-mode
          heex-ts-mode
          ruby-mode
          ruby-ts-mode
          typescript-mode
          typescript-ts-mode
          tsx-ts-mode
          ) . eglot-ensure)
  :custom
  (eglot-auto-display-help-buffer nil)
  (eglot-connect-timeout 100)
  (eglot-sync-connect 30)
  (eglot-extend-to-xref t)
  :config
  (add-to-list 'eglot-server-programs
               `((elixir-ts-mode heex-ts-mode elixir-mode) .
                 ("elixir-ls")))
  ;; (add-to-list 'eglot-server-programs
  ;;    `((elixir-ts-mode heex-ts-mode elixir-mode) .
  ;;      ("nextls" "--stdio=true" :initializationOptions (:experimental (:completions (:enable t))))))
  :bind
  (("M-RET" . eglot-code-actions)
   ("C-x C-<return>" . eglot-code-actions))
  )

(use-package transient
  :ensure t
  :config
  (load-library "transient") ;; somehow the build-in transient is
                             ;; being loaded :/ so we need to re-load
                             ;; it here
  )

(use-package magit
  ;; :straight '(magit :no-clone t) ;; installed trough home-manager
  :after (project transient)
  :bind
  (("M-M" . magit-status) ;; depreciated
   ("C-x m" . magit-status))
  :config
  (setq magit-list-refs-sortby "-committerdate")
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-diff-paint-whitespace nil)
  (setq magit-diff-highlight-hunk-body nil)
  (setq magit-diff-highlight-hunk-region-functions nil)
  )



(use-package project)

(use-package projectile
  :after (project transient)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-git-submodule-command nil)
  (projectile-mode +1)
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

(use-package yasnippet
  :hook (elixir-mode))

(use-package exunit)

(use-package ispell
  :custom
  (ispell-program-name "hunspell")
  (ispell-really-hunspell t)
  (setq ispell-extra-args "-d en_US,en_GB,pl_PL")
  )

(use-package flyspell
  :config
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

(use-package flyspell-correct-avy-menu
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after (ivy flyspell-correct)
  :after flyspell-correct)

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package ivy
  :custom
  (ivy-extra-directories nil)
  (ivy-re-builders-alist
   '((swiper . ivy--regex-plus)
     (counsel-ag . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  (ivy-use-virtual-buffers t)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-format-function #'ivy-format-function-arrow)
  :config
  (ivy-mode t))

(use-package ivy-rich
  :config
  (setq ivy-rich-path-style 'abbreviate)

  (setcdr (assq t ivy-format-functions-alist)
          #'ivy-format-function-line)
  :hook (after-init . ivy-rich-mode))

(use-package corfu
  ;; Optional customizations
  ;; :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package corfu-prescient
  :after (corfu prescient)
  :config
  (corfu-prescient-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-hook 'corfu-update-hook 'nerd-icons-corfu--update))

(use-package prescient
  :config
  (setq prescient-history-length 2000)
  (setq prescient-save-file "~/.emacs.d/prescient-items")
  (setq prescient-filter-method '(literal regexp))
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (setq ivy-prescient-sort-commands
        '(:not counsel-grep
               counsel-rg
               counsel-switch-buffer
               ivy-switch-buffer
               swiper
               swiper-multi))
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq ivy-prescient-enable-filtering nil)
  (setq ivy-prescient-enable-sorting t)
  (ivy-prescient-mode 1))


(use-package swiper
  :bind
  (([remap isearch-forward]  . swiper)
   ([remap isearch-backward] . swiper-backward)))

(use-package counsel
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ([remap yank-pop] . counsel-yank-pop))
  )

;; (use-package counsel-projectile
;;   :after (counsel projectile)
;;   :hook (projectile-mode . counsel-projectile-mode))


(use-package llm
  :init
  (require 'llm-gemini)
  :config
  (setopt llm-gemini-provider
          (make-llm-gemini :key (auth-info-password
                                 (car (auth-source-search
                                       :host "generativelanguage.googleapis.com"
                                       :user "apikey")))
                           :chat-model "gemini-1.5-flash-latest"))
  :custom
  (llm-warn-on-nonfree nil))

;; (use-package magit-gptcommit
;;   :demand t
;;   :after (llm magit)
;;   :bind (:map git-commit-mode-map
;;               ("C-c C-g" . magit-gptcommit-commit-accept))
;;   :custom
;;   (magit-gptcommit-llm-provider llm-gemini-provider)
;;   )

(use-package git-timemachine)

(use-package git-gutter)

(use-package google-translate
  :demand t
  :init
  (setq google-translate-default-source-language "pl"
        google-translate-default-target-language "en")
  :custom
  (google-translate-backend-method 'curl)
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))

  :bind
  (("C-c t T" . google-translate-at-point)
   ("C-c t t" . google-translate-query-translate)
   ("C-c t R" . google-translate-at-point-reverse)
   ("C-c t r" . google-translate-query-translate-reverse)))

;; TODO replace with something more configurable
;; pair do-end
;; pair ~~ in org-mode
(use-package autopair
  :init
  (autopair-global-mode))

(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

(use-package ace-window
  :bind
  (("C-M-'" . ace-window)))

(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer)))

(use-package ace-jump-buffer
  :bind
  (("C-\"" . ace-jump-buffer)))

(use-package expand-region
  :bind
  (("M-]" . er/expand-region)
   ("M-[" . er/contract-region)))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d> %A \n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package org-roam-timestamps
  :after org-roam
  :config (org-roam-timestamps-mode)
  )

(use-package org-roam-ui
  :after org-roam)

(use-package org-tree-slide
  :custom
  (org-image-actual-width nil))

(use-package s)

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package protobuf-mode)

(use-package rg)

(use-package beacon
  :config
  (beacon-mode 1))

(use-package terraform-mode)

(use-package typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package mocha)

(use-package nix-mode)
(use-package nix-ts-mode)

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)
(put 'scroll-left 'disabled nil)


(defun mac-srak ()
  "Usage:  `M-x anthony-kong`"
  (interactive)
  (replace-regexp "‘" "'" nil (point-min) (point-max))
  (replace-regexp "’" "'" nil (point-min) (point-max))
  (replace-regexp "“" "\"" nil (point-min) (point-max))
  (replace-regexp "”" "\"" nil (point-min) (point-max)))

