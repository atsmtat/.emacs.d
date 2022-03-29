(provide 'setup-editing)

;; Handy incremental scrolling keys
(define-key global-map "\M-n" (lambda () (interactive) (next-line 5)))
(define-key global-map "\M-p" (lambda () (interactive) (previous-line 5)))

;; goto matching counterpart of current parenthesis
(defun paren-match ()                                                  
  "Tries to jump to the matching parenthesis to the one currently      
  under the point."                                                    
  (interactive)                                                        
  (cond ((looking-at "[{\[\(]") (forward-sexp 1) (backward-char))      
        ((looking-at "[]})]") (forward-char) (backward-sexp 1))))      
(global-set-key (kbd "C-]") 'paren-match) 

;; line move up
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))
(global-set-key (kbd "M-<up>") 'move-line-up)

;; line move down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "M-<down>") 'move-line-down)

;; setup copy without selection
(defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point))
     
(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between beg & end into kill ring."
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end))))

(defun paste-to-mark (&optional arg)
  "Paste things to mark, or to the prompt in shell-mode."
  (unless (eq arg 1)
    (if (string= "shell-mode" major-mode)
        (comint-next-prompt 25535)
      (goto-char (mark)))
    (yank)))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )
(global-set-key (kbd "C-c w") 'copy-word)

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (paste-to-mark arg)
  )
(global-set-key (kbd "C-c l") 'copy-line)

;; PACKAGE: duplicate-thing
(use-package duplicate-thing
  :straight t
  :bind ("M-c" . duplicate-thing))

;; PACKAGE: volatile-highlights
(use-package volatile-highlights
  :straight t
  :config
  (volatile-highlights-mode t))

;; PACKAGE: undo-tree
(use-package undo-tree
  :straight t)
(global-undo-tree-mode)
(global-set-key (kbd "C-x >") (kbd "C-u 8 C-x ^"))
(global-set-key (kbd "C-x <") (kbd "C-u - 8 C-x ^"))

;; PACKAGE: string-inflection
(use-package string-inflection
  :straight t
  :bind (("C-c i" . string-inflection-cycle)
	 ("C-c C" . string-inflection-camelcase))
  )

;; PACKAGE: company
(use-package company
  :straight t
  :config
  (company-mode t)
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("M-<" . company-select-first)
	      ("M->" . company-select-last)))

;; bind fill region
(global-set-key (kbd "C-c C-f") 'fill-region)

;; PACKAGE: lsp-mode
(use-package lsp-mode
  :straight t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; rust-analyzer settings lifted from
  ;; https://robert.kra.hn/posts/2021-02-07_rust-with-emacs/
  ;;
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-rustc-source "discover")
  (lsp-rust-analyzer-server-command "~/.cargo/bin/rust-analyzer")

  ;; clangd settings
  ;; (lsp-clangd-binary-path "/usr/bin/clangd")
  ;; (lsp-clients-clangd-args "-j=7")
  ;; deactivate documentation showed in mini buffer
  (lsp-eldoc-hook nil)
  (lsp-idle-delay 0.6)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode 'c++-mode-hook)
  (setq lsp-clients-clangd-args '("-j=4"))
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(add-hook 'c++-mode-hook #'lsp)

;;; PACKAGE: lsp-ui
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; PACKAGE: rustic
(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; Use space instead of TAB for indentation
  (add-hook 'rustic-mode-hook
	    (lambda () (setq indent-tabs-mode nil)))
  ;; Run rustfmt upon saving
  ;; (setq rustic-format-on-save t)
  )

;; PACKAGE: flycheck
(use-package flycheck
  :straight t)

;; PACKAGE: protobuf-mode
(use-package protobuf-mode
  :straight t)
