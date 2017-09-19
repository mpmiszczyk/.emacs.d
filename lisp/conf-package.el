;; configuration for packages installed with package.erl


;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(require 'package)


(defvar required-packages
  '(ace-jump-buffer
    ace-jump-mode
    alert
    auto-complete
    autopair
    color-theme
    dash
    edts
    epc
    epl
    elpy
    elfeed
    erlang
    expand-region
    f
    flycheck
    google-translate
    hide-comnt
    ido-vertical-mode
    idomenu
    iedit
    magit
    markdown-mode
    nose
    org
    org-journal
    org-magit
    org-pomodoro
    pkg-info
    popup
    rainbow-delimiters
    rainbow-mode
    s
    smex
    ssh
    vagrant-tramp
    w3
    w3m
    yaml-mode
    yasnippet
    zenburn-theme)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))
