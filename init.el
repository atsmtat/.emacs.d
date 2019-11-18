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

;; load modules
;; load this only on arista server
(when (and (getenv "HOSTNAME") (string-match "arista" (getenv "HOSTNAME")))
  (require 'setup-arista)
);;
(require 'setup-editing)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cp-utils volatile-highlights use-package rebox2 duplicate-thing))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
