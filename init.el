;; y or n 
(defalias 'yes-or-no-p 'y-or-n-p)

;; disable menu bar
(menu-bar-mode -1)

;; bootstrap package manager straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; set up theme
(use-package zenburn-theme
  :straight t)
(load-theme 'zenburn t)

;; add custom module path
(add-to-list 'load-path "~/.emacs.d/custom/")

;; buffer management
;; use ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; load modules
;; load this only on arista server
(when (and (getenv "HOSTNAME") (string-match "arista" (getenv "HOSTNAME")))
  (require 'setup-arista)
)
(require 'setup-editing)
(require 'setup-environment)
