(provide 'setup-editing)

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



;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: rebox2 ;;
;;                 ;;
;; GROUP: editing  ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package rebox2
  :ensure t
  :init
  (setq rebox-style-loop '(21 25 10))
  :bind (("M-q" . rebox-dwim)
	 ("S-M-q" . rebox-cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: duplicate-thing ;;
;;                          ;;
;; GROUP: editing           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package duplicate-thing
  :ensure t
  :bind ("M-c" . duplicate-thing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: volatile-highlights ;;
;;                              ;;
;; GROUP: editing               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: undo-tree ;;
;;                    ;;
;; GROUP: editing     ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package undo-tree
;;   :ensure t)
;; (global-undo-tree-mode)
;; (global-set-key (kbd "C-x >") (kbd "C-u 8 C-x ^"))
;; (global-set-key (kbd "C-x <") (kbd "C-u - 8 C-x ^"))


;; (use-package cp-utils
;;   :ensure t)
;; (global-set-key (kbd "C-x c w") 'cp-cur-word)
;; (global-set-key (kbd "C-x c f") 'cp-cur-file-name)
;; (global-set-key (kbd "C-x c h") 'cp-which-func)
;; (global-set-key (kbd "C-x c b") 'cp-which-func-and-cur-file-name)
