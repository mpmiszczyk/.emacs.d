

(use-package org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("C-'" . avy-goto-char-timer))

  :config
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/inbox.org"))
  (setq org-log-done t)

  (setq org-drill-learn-fraction 0.39)


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

(use-package org-journal
  :after (org-mode)
  :custom
  (org-journal-dir (concat org-directory "/journal/"))
  (org-journal-tag-alist '(("personal" . ?p)
                           ("dreams" . ?D)
                           ("memories" . ?m)
                           ("summary" . ?s)
                           ("writing" . ?W)
                           ("craft" . ?c)
                           ("work" . ?w)
                           ("daily" . ?d)
                           ("fresha" . ?f))))


(provide 'org-mode-config)
