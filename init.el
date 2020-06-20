;; y or n 
(defalias 'yes-or-no-p 'y-or-n-p)

;; add MELPA package archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; install use-package if it's not installed already
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; add custom module path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; set up theme
(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)

;; load modules
;; load this only on arista server
(when (and (getenv "HOSTNAME") (string-match "arista" (getenv "HOSTNAME")))
  (require 'setup-arista)
)
(require 'setup-editing)
(require 'setup-environment)
