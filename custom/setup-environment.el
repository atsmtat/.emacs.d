(provide 'setup-environment)

(setq scroll-step            1
      scroll-conservatively  10000)

;; PACKAGE: ido
(require 'ido)

(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)

(ido-mode 1)

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
