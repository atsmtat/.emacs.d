(provide 'setup-editing)

;; add line numbers except no shell and compilation buffers 

;; (global-linum-mode 1)
;; (setq linum-format "%d ")
;; (setq linum-disabled-modes-list '(eshell-mode wl-summary-mode compilation-mode shell-mode))
;; (defun linum-on ()
;;   (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
;;     (linum-mode 1)))

;; Handy incremental scrolling keys
(define-key global-map "\M-n" (lambda () (interactive) (next-line 5)))
(define-key global-map "\M-p" (lambda () (interactive) (previous-line 5)))

;; Highlight the mode line of active buffer orange
(set-face-background 'mode-line "white")
(set-face-foreground 'mode-line "blue")
(set-face-foreground 'modeline-inactive "black")
(set-face-background 'modeline-inactive "white")

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
