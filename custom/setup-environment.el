(provide 'setup-environment)

;; shell setup
(defun my-comint-send-input-maybe ()
  "Only `comint-send-input' when point is after the latest prompt.
Otherwise move to the end of the buffer."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (and proc (>= (point) (marker-position (process-mark proc))))
        (comint-send-input)
      (goto-char (point-max)))))

(with-eval-after-load "comint"
  (define-key shell-mode-map [remap comint-send-input] 'my-comint-send-input-maybe))

;; PACKAGE: org
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
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
