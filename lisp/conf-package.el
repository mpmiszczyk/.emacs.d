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
  '(avy
    autopair
    erlang
    expand-region
    f
    flycheck
    google-translate
    magit
    markdown-mode
    org
    org-journal
    popup ; to be removed
    rainbow-delimiters
    smex ; to be removed
    speed-type
    ssh ; not sure needed
    yaml-mode
    yasnippet
    zenburn-theme)
  "Packages which should be installed upon launch")

(dolist (p required-packages)
  (when (not (package-installed-p p))
    (package-install p)))
