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

;; disable menu bar
(menu-bar-mode -1)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (magit zenburn-theme volatile-highlights use-package undo-tree string-inflection spacemacs-theme rust-mode rebox2 duplicate-thing ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
