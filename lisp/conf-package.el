;; configuration for packages installed with package.erl


(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

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
    w3
    w3m
    yaml-mode
    yasnippet )
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))
