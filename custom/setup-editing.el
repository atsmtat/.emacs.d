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
(global-set-key (kbd "ESC <up>") 'move-line-up)

;; line move down
(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))
(global-set-key (kbd "ESC <down>") 'move-line-down)

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
  :ensure t
  :bind ("M-c" . duplicate-thing))

;; PACKAGE: volatile-highlights
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;; PACKAGE: undo-tree
(use-package undo-tree
  :ensure t)
(global-undo-tree-mode)
(global-set-key (kbd "C-x >") (kbd "C-u 8 C-x ^"))
(global-set-key (kbd "C-x <") (kbd "C-u - 8 C-x ^"))

;; PACKAGE: string-inflection
(use-package string-inflection
  :ensure t
  :bind (("C-c i" . string-inflection-cycle)
	 ("C-c C" . string-inflection-camelcase))
  )

;; PACKAGE: rust-mode
(use-package rust-mode
  :ensure t )
