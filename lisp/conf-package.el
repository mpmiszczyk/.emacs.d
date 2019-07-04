;; configuration for packages installed with straight.el

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(let ((required-packages
       '(erlang
         expand-region
         f
         flycheck
         markdown-mode
         speed-type
         ssh ; not sure needed
         yaml-mode
         yasnippet
         zenburn-theme)))
  "Packages which should be installed upon launch"
  (dolist (package required-packages)
    (use-package package)))


