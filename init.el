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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-allow-recipe-inheritance t)

(setq custom-file (make-temp-file "emacs-custom"))
(load custom-file)

(use-package emacs
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (setq inhibit-startup-message t)
  (setq visible-bell t)
  (global-hl-line-mode)
  (setq ruby-insert-encoding-magic-comment nil)
  )

;; TODO fixes: var used way before it is introduced.  It seems it is
;; used in `project.el', but it needs to be defined way sooner.
;; Remove at some point in future.
(unless (boundp 'tab-prefix-map)
  (defvar tab-prefix-map (make-sparse-keymap)))

;; TODO should be moved somewhere sensible
(use-package ansi-color ; library needed to be loaded for `ansi-color-apply-on-region'
  :config
  (defun use-ansi-colors ()
      (interactive)
    (ansi-color-apply-on-region (point-min) (point-max))))

(use-package nord-theme)

(use-package modus-themes)

(use-package zenburn-theme)

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi 'no-confirm))

;;(set-default-font “Lekton-9”)
;;(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")
(set-face-attribute 'default nil :font "Iosevka ss04-12")
;;(set-face-attribute 'default nil :font "Iosevka-10") ;gggg

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse nil) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")


;; note to myself:
;; ediff docs suck
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path "~/.emacs.d/lisp")

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

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
  :config
  (setq erlang-indent-level 2)
  )

(use-package elixir-mode)

;; (straight-use-package
;;   '(flymake :fork "mpmiszczyk/flymake"))

;; (flymake :type git :host github :repo "emacs-straight/flymake" :files ("*" (:exclude ".git")))

(use-package flymake
  :straight (:fork "mpmiszczyk/flymake" :branch "pre-string-replace"))

(use-package eglot
  :commands (eglot eglot-ensures)
  :hook
  (elixir-mode . eglot-ensure)
  (ruby-mode . eglot-ensure)
  (before-save . eglot-format-buffer)
  :config
  (setq eglot-auto-display-help-buffer nil)
  ;; (add-to-list
  ;;  `eglot-server-programs `(ruby-mode "solargraph" "socket" "--port" :autoport))
  (add-to-list
   'eglot-server-programs '(elixir-mode "/home/mpm/elixir_ls/release/language_server.sh")))

(use-package project
  :config
  (defun project-root (project)
    (car (project-roots project))))

(use-package projectile
  :after project
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode +1)
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

(use-package yasnippet
  :hook (elixir-mode . yas-minor-mode))

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

(use-package flyspell-correct
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

(use-package magit
  :bind
  ("M-M" . magit-status)
  :config
  (setq magit-list-refs-sortby "-committerdate")
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-timemachine)

(use-package git-gutter)

(use-package google-translate
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


;; TODO move to better place
(global-set-key (kbd "<C-tab>") 'other-window)

(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

(use-package avy
  :bind
  (("C-'" . avy-goto-char-timer)
   ("C-M-'" . avy-goto-subword-or-word-1)))

(use-package ace-jump-buffer
    :bind
    (("C-\"" . ace-jump-buffer)))

(use-package expand-region
  :bind
  (("M-]" . er/expand-region)
   ("M-[" . er/contract-region)))

(require 'org-mode-config)

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

(use-package elfeed)
(use-package s)

(use-package elfeed-org
  :config
  (setq rmh-elfeed-org-files (list "~/.emacs.d/lisp/elfeed.org"))
  :init
  (elfeed-org)
  :after (elfeed s)
)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package git-link)

(use-package protobuf-mode)

(use-package ripgrep)

(use-package beacon
  :config
  (beacon-mode 1))

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
