(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes (quote ("47583b577fb062aeb89d3c45689a4f2646b7ebcb02e6cb2d5f6e2790afb91a18" "f52632eabbcbdccfb5070dfa864fd8f6cab7ad5db5bb027a385613fe955a477d" "6872b9ccad2ef1b4a12644f37169eade1b567f53b18737d6914168ef3f02a959" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "60a2ebd7effefeb960f61bc4772afd8b1ae4ea48fae4d732864ab9647c92093a" "2ff493cb70e33443140cd5286553d994f25478182a8c20382895f452666c20c6" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "9f443833deb3412a34d2d2c912247349d4bd1b09e0f5eaba11a3ea7872892000" "52b5da0a421b020e2d3429f1d4929089d18a56e8e43fe7470af2cea5a6c96443" default)))
 '(edts-man-root "~/.emacs.d/edts/doc/R16B03")
 '(org-agenda-files (quote ("~/org/inbox.org" "~/org/inbox.org_archive" "~/org/own/sckrk.org" "~/org/own/rower.org" "~/org/own/craft.org" "~/org/own/films.org" "~/org/own/priorytety.org" "~/org/ESL/ParaPhrase/Skel.org" "~/org/ESL/ParaPhrase/Exago.org" "~/org/ESL/ParaPhrase/emas.org" "~/org/ESL/ParaPhrase/calls.org" "~/org/ESL/ParaPhrase/Skel.org_archive" "~/org/ESL/ParaPhrase/ParaPhrase.org" "~/org/ESL/praca.org" "~/org/ESL/erlang.org_archive" "~/org/ESL/erlang.org" "~/org/system/emacs.org_archive" "~/org/system/system.org" "~/org/system/emacs.org" "~/org/studia/studia.org" "~/org/data/3f/8390d4-41d9-4c9a-a646-dad21cec1fe9/erlang.org" "~/org/data/cf/3db1ee-bd41-4f1b-9c6e-53778947ed6a/erlang.org" "/home/bs/org/inbox.org"))))


(require 'package)
(add-to-list 'package-archives 
             '("marmalade" ."http://marmalade-repo.org/packages/") )
(add-to-list 'package-archives 
             '("melpa" . "http://melpa.milkbox.net/packages/") )
(package-initialize)


(elpy-enable)



(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(autopair-global-mode)
(global-rainbow-delimiters-mode)
(ido-vertical-mode)

(setq org-pomodoro-play-ticking-sounds t)

;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse nil) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; (set-default-font “Lekton-9”)
(set-face-attribute 'default nil :font "DejaVu Sans Mono-7.7")

(load-theme 'molokai)

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

(global-set-key (kbd "M-?") 'comment-or-uncomment-region)

(global-set-key "\M-," 'pop-to-mark-command)
(global-set-key "\M-w" 'er/expand-region)
(global-set-key "\M-W" 'er/contract-region)

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
             (org-indent-mode)))

(require 'org-journal)
(setq org-journal-dir "~/org/journal/")

(setq org-agenda-files   
      (mapcar 'abbreviate-file-name
              (split-string
               (shell-command-to-string "find ~/org -name \"*.org\" -o -name \"*.org_archive\""  ) "\n")))

;; (global-set-key TODO something lost -> need to start cersion control



(add-to-list 'load-path "~/.emacs.d/opt/edts")
(require 'edts-start)

(add-to-list 'load-path "/usr/local/lib/erlang/lib/wrangler-1.1.01/elisp")
(require 'wrangler)


(setq erlang-indent-level 2)
;; (require 'erlang-flymake)
;; diabeld, becouse it wasn't usefull
;; and it was imposible to set-up (dependencis)


;; (add-hook 'erlang-mode '(lambda () 
;;                           ( hs-minor-mode ) 
;;                           ;;TODO automatyczne włączanie modaoraz
;;                           ;;indentowanie wszystkich kometarzy
;;                           (dired-omit-mode 1)
;;                           ;; TODO trzeba dodać nadpisywanie glabalnych
;;                           ;; key; bo normalnie "M-m" używam do
;;                           ;; wywoływania make 
;;                           (local-set-key (kbd  "M-m")
;;                                          'edts-code-compile-and-display ) 
;;                           (local-set-key (kbd
;;                                           "M-r") 'edts-find-local-function) ))

;; (add-hook 'erlang-mode-hook '(lambda ()
;;       (local-set-key (kbd "RET") 'newline-and-indent))
;;           )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  NOTES  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; auto-compleate to "\M-/"
;;
;; TODO global-hl-line-mode
;; TODO autopair-global-mode
;; TODO global-rainbow-mode
;; TODO global-hs-mode
;; TODO better buffers from bambucha
;; TODO hs-mode
;; DONE wright font
;; TODO czerwone komentarze; bo takie są zupełnie nie czytelne

;; TODO doinstalowac:
;; hs-mode
;; zaznaczanie coraz wiekszych
;;
;; 

;; Działa równierz przechodzenie pomiędzy buforami przy pomocy
;; nex/prev na klawiaturze (to te dwa trzyciski koło strzałek
;;
;; "M-Q automatycznie wypełnia zadane linikji do danej długości;
;; przydatne przy komentarzach ale przy zwykłym kodzie mocno psuje
;; strykturę
;;
;; "M-j" indentuje nową linię w komentachach,
;; "C-j" nowa linia i indent wszędzie
;;
;; istnieje polecenie w edts które pozwala na kopiwanie argumentów
;; powtarzanej funkcji, można by przypisać, lub przynajmniej o tym
;; pamiętać
;;
;;
;; "C-x k" zabija daby buffer
;; i jeszcze zamin zapomnę, magit jest super, org jest powyżej
;; oczekiwań; emacs mnie wicągnoł na całego.
;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
