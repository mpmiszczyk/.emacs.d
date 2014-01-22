(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

;; (set-default-font “Lekton-9”)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-8.7")


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn")
(load-theme 'zenburn 'no-confirm)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


(add-to-list 'load-path "~/.emacs.d")

(load "conf-package")


(elpy-enable)

(require 'google-translate)
(setq google-translate-default-source-language "pl")
(setq google-translate-default-target-language "en")

(global-set-key (kbd "C-c t T")
                'google-translate-at-point)
(global-set-key (kbd "C-c t t")
                'google-translate-query-translate)

(global-set-key (kbd "C-c t R")
                'google-translate-at-point-reverse)
(global-set-key (kbd "C-c t r")
                'google-translate-query-translate-reverse)


(autopair-global-mode)
(global-rainbow-delimiters-mode)
(global-hl-line-mode)
(ido-vertical-mode)


;; note to myself:
;; ediff docs suck
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse nil) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "<C-tab>") 'other-window)


;; jakoś nie chce to mi działać :/
(setq tramp-syntax 'url)
(setq tramp-default-method "ssh")

;; (require 'conf-mu4e)

(require 'tramp)

(defun find-alternative-file-with-sudo ()
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'find-alternative-file-with-sudo)

                                        ;(load-file "/media/own/opt/cedet-1.1/common/cedet.el")
                                        ;(global-ede-mode 1)                      ; Enable the Project management system
                                        ;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
                                        ;(global-srecode-minor-mode 1)            ; Enable template insertion menu


(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [(shift meta x)] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))


;; Lisp Flavored Erlang mode
;; Set lfe-dir to point to where the lfe emacs files are.
                                        ;(defvar lfe-dir "/media/own/opt/lfe/emacs")
                                        ;(setq load-path (cons lfe-dir load-path))
                                        ;(require 'lfe-start)


(global-set-key "\M-m"   'compile)  ;; m is for make; TODO oraz
;; przydało by się rozdzielić
;; automatyczny make z pierszką
;; opcją (lub ostatnią) i make i
;; wywoływaną opcją
(global-set-key [M-down] 'next-error)
(global-set-key [M-up]   '(lambda ()
                            (interactive)
                            (next-error -1)))


(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-'") 'ace-jump-buffer)


(global-set-key (kbd "M-?") 'comment-or-uncomment-region)

(global-set-key "\M-," 'pop-to-mark-command)
(global-set-key "\M-]" 'er/expand-region)
(global-set-key "\M-[" 'er/contract-region)

(global-set-key "\M-=" 'hs-toggle-hiding)
(global-set-key (kbd "C-M-=") 'hs-hide-all) 


(require 'org-mode-config)



(add-to-list 'load-path "~/.emacs.d/opt/edts")
(require 'edts-start)



(setq erlang-indent-level 2)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
