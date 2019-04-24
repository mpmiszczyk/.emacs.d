;; configuration for packages installed with straight.el

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(let ((required-packages
       '(erlang
         expand-region
         f
         flycheck
         google-translate
         magit
         markdown-mode
         org
         rainbow-delimiters
         speed-type
         ssh ; not sure needed
         yaml-mode
         yasnippet
         zenburn-theme)))
  "Packages which should be installed upon launch"
  (dolist (package required-packages)
    (straight-use-package (quote package))))


