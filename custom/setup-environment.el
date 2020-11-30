(provide 'setup-environment)

(setq scroll-step            1
      scroll-conservatively  10000)

;; PACKAGE: org
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files ( list "~/Org/Projects.org" ) )

;; PACKAGE: magit
(use-package magit
  :ensure t)
(global-set-key (kbd "C-x g") 'magit-status)

;; PACKAGE: ace-window
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

;; PACKAGE: helm
(use-package helm
  :ensure t)
(require 'helm-config)

;; rebind helm-command-prefix from "C-x c" to "C-c h"
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; list actions using C-j
(define-key helm-map (kbd "C-j")  'helm-select-action)

(setq helm-split-window-in-side-p t )

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)

;; PACKAGE: markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
