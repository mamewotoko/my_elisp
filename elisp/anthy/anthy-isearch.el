;; anthy-isearch.el

;; DO NOT USE NOW.
;;

;;; Commentary:
;; TOOOOOOOOOOOOOOOOOOOO many things to be implemented.
;; most of the code is stolen from SKK.

(defvar anthy-isearch-mode-map
  ())

(or
 anthy-isearch-mode-map
 (let ((map (make-sparse-keymap))
       (i 32))
   (while (< i 127)
     (define-key map (char-to-string i) 'kill-emacs)
     (setq i (+ 1 i)))
   (setq anthy-isearch-mode-map map)))

(defun anthy-isearch-mode-setup ()
  (set-keymap-parent anthy-isearch-mode-map isearch-mode-map)
  ())

(defun anthy-isearch-mode-cleanup ()
  ())

(add-hook 'isearch-mode-hook 'anthy-isearch-mode-setup)

