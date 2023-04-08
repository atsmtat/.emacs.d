(provide 'setup-arista)

;; load Arastra library
(setq arastra-server-start nil)
(load-library "Arastra")

(global-set-key (kbd "C-c e") 'a4-edit)

;; a4-gid bindings
;; (global-set-key (kbd "M-[") 'a4-gid-defs)
;; (global-set-key (kbd "M-]") 'a4-gid)
;; (global-set-key (quote [M-return]) (quote a4-gid-repeat))
;; (global-set-key (kbd "M-'") 'a4-gid-go-back)
;; (global-set-key (kbd "M-;") 'a4-gid-kill)

;; (global-set-key (quote [M-up]) (quote previous-error))
;; (global-set-key (quote [M-down]) (quote next-error))
;; (setq a4-gid-highlight-target)

;; Disable aformat autocorrect
(setq aformat-autocorrect nil)

;; support imenu navigation in TAC mode
(add-hook 'tac-mode-hook
          (lambda()
            (local-set-key "\M-j" 'a4-nav-imenu-prev)
            (local-set-key "\M-k" 'a4-nav-imenu-next)
            ))

;; support imenu navigation in C++ mode
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key "\M-j" 'a4-nav-imenu-prev)
            (local-set-key "\M-k" 'a4-nav-imenu-next)
            ))

;; occur buffer containing classes and function definitions in python file
(defun occur-python-defs ()
  (interactive)
  (occur "^\\s-*\\(class\\|def\\)\\s-"))

(add-hook 'python-mode-hook 
          (lambda ()
            (local-set-key (kbd "C-c d") 'occur-python-defs)
            ))

;; Indicate a max column vertical line
;; (require 'fill-column-indicator)
;; (setq fci-rule-width 1)
;; (setq fci-rule-color "lightblue")
;; (setq-default fill-column 85)
;; (add-hook 'after-change-major-mode-hook 'fci-mode)


(require 'bug-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(tac-mode . "tac"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "artaclsp")
                    :major-modes '(tac-mode)
                    :server-id 'artaclsp)))

(add-hook 'tac-mode-hook #'lsp)

