(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)

;; (set-default-font “Lekton-9”)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-7.7")


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn")
(load-theme 'zenburn 'no-confirm)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("3c0cab34db95a5368ccc63fe09b852450f2ab3f7436acb70611f5410f32eb71e" "40e82796094b711c413904950f8b11f883d5481ad5070d316384f71e852f5124" "beeb3e484a76bd5fd3e259bd0aaa6fb56ecc306a97d951d27e40dfea033b2dba" "43ccb5e4783c7171e4361f617fad7bdec02050a010e20903a978a4a1b97862c5" "d070fa185078bf753dcfd873ec63be19fa36a55a0c97dc66848a6d20c5fffdad" "c739f435660ca9d9e77312cbb878d5d7fd31e386a7758c982fa54a49ffd47f6e" "968d1ad07c38d02d2e5debffc5638332696ac41af7974ade6f95841359ed73e3" "8eef22cd6c122530722104b7c82bc8cdbb690a4ccdd95c5ceec4f3efa5d654f5" "47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "f52632eabbcbdccfb5070dfa864fd8f6cab7ad5db5bb027a385613fe955a477d" "6872b9ccad2ef1b4a12644f37169eade1b567f53b18737d6914168ef3f02a959" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "2ff493cb70e33443140cd5286553d994f25478182a8c20382895f452666c20c6" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "52b5da0a421b020e2d3429f1d4929089d18a56e8e43fe7470af2cea5a6c96443" default)))
 '(edts-man-root "~/.emacs.d/edts/doc/R16B03")
 '(fci-rule-color "#383838")
 '(global-hl-line-mode nil)
 '(org-agenda-files (quote ("~/org/inbox.org" "~/org/inbox.org_archive" "~/org/own/rower.org" "~/org/own/films.org" "~/org/own/priorytety.org" "~/org/ESL/ParaPhrase/Skel.org" "~/org/ESL/ParaPhrase/Exago.org" "~/org/ESL/ParaPhrase/emas.org" "~/org/ESL/ParaPhrase/Skel.org_archive" "~/org/ESL/ParaPhrase/ParaPhrase.org" "~/org/ESL/calls.org" "~/org/ESL/praca.org" "~/org/studia.org" "~/org/data/3f/8390d4-41d9-4c9a-a646-dad21cec1fe9/erlang.org" "~/org/data/cf/3db1ee-bd41-4f1b-9c6e-53778947ed6a/erlang.org" "~/org/progr/emacs.org_archive" "~/org/progr/apps.org" "~/org/progr/system.org" "~/org/progr/craft.org" "~/org/progr/emacs.org" "~/org/progr/erlang.org_archive" "~/org/progr/erlang.org" "/home/bs/org/own/films.org" "/home/bs/org/own/priorytety.org" "/home/bs/org/own/rower.org")))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map (quote ((20 . "#BC8383") (40 . "#CC9393") (60 . "#DFAF8F") (80 . "#D0BF8F") (100 . "#E0CF9F") (120 . "#F0DFAF") (140 . "#5F7F5F") (160 . "#7F9F7F") (180 . "#8FB28F") (200 . "#9FC59F") (220 . "#AFD8AF") (240 . "#BFEBBF") (260 . "#93E0E3") (280 . "#6CA0A3") (300 . "#7CB8BB") (320 . "#8CD0D3") (340 . "#94BFF3") (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))


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

(add-to-list 'load-path "~/.emacs.d")
(require 'conf-mu4e)

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


(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-log-done t)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)
             (auto-fill-mode)
             (org-indent-mode)
             (rainbow-delimiters-mode-disable)))

(require 'org-journal)
(setq org-journal-dir "~/org/journal/")

(setq org-pomodoro-play-ticking-sounds t)

(steq org-clock-into-drawer t)

(setq org-agenda-files   
      (mapcar 'abbreviate-file-name
              (split-string
               (shell-command-to-string "find ~/org -name \"*.org\" -o -name \"*.org_archive\""  ) "\n")))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 2)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

(setq org-refile-allow-creating-parent-nodes nil)

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)

; normal yank will act as smart one in org-mode
(setq org-yank-adjusted-subtrees t)

(add-to-list 'org-modules 'org-habit)


;; (global-set-key TODO something lost -> need to start cersion control



(add-to-list 'load-path "~/.emacs.d/opt/edts")
(require 'edts-start)



(setq erlang-indent-level 2)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
