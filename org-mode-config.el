(setq org-directory "~/Dropbox/org")
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
  (smex-save-to-file))



(require 'org-journal)
(setq org-journal-dir (concat org-directory "/journal/"))

(setq org-pomodoro-play-ticking-sounds t)

(setq org-clock-into-drawer t)

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

;; (add-to-list 'org-modules 'org-habit)



(provide 'org-mode-config)
