
;; y or n 
(defalias 'yes-or-no-p 'y-or-n-p)

;; add MELPA package archive
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (misterioso)))
 '(ediff-combination-pattern
   (quote
    (">>>> ORIGINAL" Ancestor "==== THEIRS" B "==== YOURS" A "<<<<")))
 '(indent-tabs-mode nil)
 '(package-selected-packages (quote (duplicate-thing rebox2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: rebox2 ;;
;;                 ;;
;; GROUP: editing  ;;
;;;;;;;;;;;;;;;;;;;;;
(setq rebox-style-loop '(21 25 10))
(require 'rebox2)
(global-set-key [(meta q)] 'rebox-dwim)
(global-set-key [(shift meta q)] 'rebox-cycle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: duplicate-thing ;;
;;                          ;;
;; GROUP: editing           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: volatile-highlights ;;
;;                              ;;
;; GROUP: editing               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGE: undo-tree ;;
;;                    ;;
;; GROUP: editing     ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)

(global-set-key (kbd "C-x >") (kbd "C-u 8 C-x ^"))
(global-set-key (kbd "C-x <") (kbd "C-u - 8 C-x ^"))

(require 'cp-utils)
(global-set-key (kbd "C-x c w") 'cp-cur-word)
(global-set-key (kbd "C-x c f") 'cp-cur-file-name)
(global-set-key (kbd "C-x c h") 'cp-which-func)
(global-set-key (kbd "C-x c b") 'cp-which-func-and-cur-file-name)
