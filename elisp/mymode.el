;;; mymode.el --- Last modified: Mon Sep 11 20:25:20 2017
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

;; 2003/ 2/ 5 gdb のエラージャンプを追加。エラージャンプを大幅改造
;;   myerrorjump.el にジャンプルーチンを入れた。

;; namazu indexes
(defconst emacs-namazu-dir "/home/tak/.Elisp_namazu")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
;; 
(defun my-Info-search-next ()
  (interactive)
  (Info-search Info-last-search))
(require 'info)
(define-key Info-mode-map "\C-n" 'my-Info-search-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML mode
;;
;;
(load "myhtmlmode.el")
(defvar my-html-image-scale 0.4)
(add-hook 'html-mode-hook
	  '(lambda ()
	     (define-key html-mode-map [(control space)]
	       'my-html-insert-space)
))

(defun my-html-copy-format-command-and-other ()
  (interactive)
  (if (tooltip-region-active-p)
      (let ((here (point))
	    (start (region-beginning))
	    (end (region-end)))
	(my-copy-primary-selection-and-add-to-my-incremental start end)
	(my-html-copy-format-register-function start end)
	(goto-char here))))

(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")

(custom-set-variables
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128,D103,E501,D100"))))

(add-hook 'org-mode-hook '(lambda () (require org-ditaa)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mew mode
;;
;;
;; (add-hook 'mew-message-mode-hook
;; 	  '(lambda ()
;; 	     (define-key mew-message-mode-map [(shift button2)]
;; 	       'browse-url-at-mouse)))
;; (add-hook 'mew-summary-mode-hook
;; 	  '(lambda ()
;; 	     (make-local-variable 'namazu-default-dir)
;; 	     (setq namazu-default-dir (expand-file-name "~/.Mail_namazu"))))

;(add-hook 'mew-draft-mode-hook
;	  '(lambda ()
;	     (auto-fill-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode
;;
;;
;(load "mycextend.el")

;(load "mycextend.el")
;(add-hook 'c-mode-hook
;	  '(lambda ()
;	     (define-key c-mode-map "\C-x\C-h" 'my-c-open-header-and-source)
;	     (setq namazu-default-dir "/home/tak/.indexes/gtk2.0")
;	     (setq c-basic-offset 4)
;	     (setq tab-width 4)
;	     (setq font-lock-keywords c-font-lock-keywords-2)
;	     (define-key c-mode-map "\C-c\C-c" 'comment-region)
;	     ))

;; ctypes.el
;(add-hook 'c++-mode-hook
;	  '(lambda ()
;	     (setq font-lock-keywords c++-font-lock-keywords-2)
;	     (define-key c++-mode-map "\C-c\C-c" 'comment-region)
;	     (define-key c++-mode-map "\C-j" 'c++-insert-incremental-for)
;	     (define-key c++-mode-map "\C-cj" 'c-insert-conditional-compile)
;	     (define-key c++-mode-map [f6] 'mycompile-compile)
;	     (define-key c++-mode-map "\C-cm" 'my-c++-extend-menu)
;	     (define-key c++-mode-map "\C-c\C-d" 'delete-rectangle)
;	     (define-key c++-mode-map "\C-c\C-t\C-y" 'my-increment-repeat)
;	     (define-key c++-mode-map "\C-b\C-b" 'my-c++-open-header-and-source)
;	     (define-key c++-mode-map "\C-c\C-b" 'my-c-prototype-to-implementation-repeat)
;	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROLOG mode
;;
;;
;(add-hook 'prolog-mode-hook 
;	  '(lambda ()
;	     (setq tab-width 2)
;	     (define-key prolog-mode-map "\C-c\C-c" 'comment-region)
;	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell Script mode
;;
;;
(add-hook 'shell-script-mode-hook
	  '(lambda ()
	     (define-key sh-mode-map "\C-c\C-c" 'comment-region)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp mode
;;
;;
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map "\C-c\C-c" 'comment-region)
	     (define-key emacs-lisp-mode-map "\C-j" 'eval-print-last-sexp)
	     (make-local-variable 'namazu-default-dir)
	     (setq namazu-default-dir emacs-namazu-dir)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Makefile mode
;;
;;
(add-hook 'makefile-mode-hook
	  '(lambda ()
	     (define-key makefile-mode-map "\C-c\C-c" 'comment-region)
	     (define-key makefile-mode-map "\M-n" 'find-include-etc-at-position)))

(defun my-opa-jump-to-error-point (error-message)
  (and (string-match "^File \"\\([^\"]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)" error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message))
	     (char-number (match-string 3 error-message)))
	 (my-error-jump-to-point filename line-number char-number))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell mode
;;
(load "mycextend.el")
(load "mycamlextend.el")
(defun my-goto-error ()
  (interactive)
  (beginning-of-line)
  (let ((start-point (point)))
    (end-of-line)
    (let* ((end-point (point))
	  (error-message (buffer-substring start-point end-point)))
      (or 
;       (my-promela-jump-to-error-point error-message)
       (my-caml-jump-to-error-at-point-entry-point error-message)
;       (my-opa-jump-to-error-point error-message)
       (my-c-jump-to-error-point-sub error-message t)
       (my-perl-jump-to-error-point error-message)
       (and (boundp 'shell-dirstack) (my-java-jump-at-exception error-message shell-dirstack))
					; (my-xvcg-jump-to-error error-message)
					; (my-platex-jump-to-error error-message)
       (my-gdb-jump-to-point-sub error-message)
	  (progn (message "no error found") (goto-char start-point) nil)))))

;(require 'dirtrack)

(add-hook 'shell-mode-hook
	  '(lambda ()
	     (define-key shell-mode-map
	       "\C-p"
	       'comint-previous-input)
	     (define-key shell-mode-map
	       "\C-n"
	       'comint-next-input)
	     (define-key shell-mode-map [f12] 'dirs)
	     (define-key shell-mode-map
	       "\C-c\C-j" 'my-goto-error)
	     (shell-dirtrack-mode 1)
             (setq dirtrack-list '(":*\\([A-Za-z]*:*~*[\/\\].*?\\)[^-+A-Za-z0-9_.()//\\ ]" 1)) ;for help making this regular expression you may want to use "M-x re-builder", where M is usually alt
             ;(dirtrack-mode)
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; caml mode
;;
;;
;(require 'caml)
;(require 'caml-font)
;(load "mycamlapi.el")

;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
;(defvar my-ocaml-manual-url "file:///home/tak/Doc/htmlman/index.html")
;(require 'search_ocaml_type)

;(add-hook 'caml-mode-hook
;	  '(lambda ()
;	     (make-local-variable 'namazu-default-dir)
;;	     (setq namazu-default-dir 
;;		   (concat (expand-file-name "~/.ocaml_namazu") " " (expand-file-name "~/.labltk_namazu")))
;	     (define-key caml-mode-map "\C-c\C-c" 'comment-region)
;	     (define-key caml-mode-map [(shift f5)] 'my-browse-caml-api-at-position)
;	     (define-key caml-mode-map [button2] 'mouse-yank)
;	     (define-key caml-mode-map [f5] 
;	       '(lambda () (interactive) 
;		  (let ((word (read-input "variable: ")))
;		    (if (equal word "")
;			(browse-url my-ocaml-manual-url)
;		      (my-browse-caml-api-from-word word)))))
;	     (define-key caml-mode-map [(control f5)] 
;	       '(lambda () (interactive)
;		  (message "t)ype or v)alue or a)ll?: ")
;		  (let* ((searchtype (read-char)))
;		    (cond ((eq searchtype ?t)
;			   (my-search-ocaml-type))
;			  ((eq searchtype ?v)
;			   (my-search-ocaml-value))
;			  (t (my-search-ocaml-all))))))
;	     ))

;(add-hook 'inferior-caml-mode-hook
;	  '(lambda ()
;;	     (define-key inferior-caml-mode-map
;;	       "\C-c\C-c" 'comment-region)
;	     (define-key inferior-caml-mode-map
;	       [(shift f5)] 'my-browse-caml-api-at-region)
;	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; comint mode
;;
;;
(require 'comint)
(add-hook 'comint-mode-hook
	  '(lambda ()
	     (define-key comint-mode-map
	       "\C-p"
	       'comint-previous-input)
	     (define-key comint-mode-map
	       "\C-n"
	       'comint-next-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode
;;
;;
;(require 'comint)
(load "myperlextend.el")

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map
	       "\C-h"
	       'backward-delete-char)
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDE mode
;;
;;
;(load "myjavamode.el")
;(load "myjavaapisearch.el")

(add-hook 'java-mode-hook
	  '(lambda ()
	     (setq tab-width 4)))

;(add-hook 'help-mode-hook
;	  '(lambda ()
;	     (define-key help-mode-map
;	       [return]
;	       'apropos-follow
;	     )))

;(add-hook 'flyspell-mode-hook
;	  '(lambda ()
;	     (define-key flyspell-mode-map
;	       "\C-n"
;	       'flyspell-goto-next-error)))

(add-hook 'flymake-mode-hook
	  '(lambda ()
             (flymake-python-pyflakes-load)
	     (define-key flymake-mode-map
	       [(control <)]
	       'flymake-goto-previous-error)
	     (define-key flymake-mode-map
	       [(control >)]
	       'flymake-goto-next-error)
             ))

(add-hook 'calendar-mode-hook
	  '(lambda ()
	     (define-key calendar-mode-map
	       "\C-v" (other-window 1))))

;(add-hook 'view-mode-hook
;	  '(lambda ()
;	     (my-vi-bind view-mode-map)))

(require 'ssh)
(setq ssh-directory-tracking-mode 'ftp)
(add-hook 'ssh-mode-hook
          '(lambda ()
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
           '(lambda ()
              (flyspell-mode)))

(setq auto-mode-alist
      (append (list
;	       (cons "\\.tex$" 'yatex-mode)
;		    (cons "\\.h$" 'c-mode)
;		    (cons "\\.jj$" 'java-mode)
;		    (cons "\\.jgo$" 'java-mode)
	       (cons "\\.cgi$" 'cperl-mode)
;	       (cons "\\.R$" 'ess-mode)
		    (cons "\\.ml[iylp]?$" 'tuareg-mode)
		    (cons "\\.sql$" 'sql-mode)
		    (cons "\\.opa$" 'opa-classic-mode)
;		    (cons "\\.scope$" 'java-mode)
		    (cons "\\.java$" 'java-mode)
                    (cons "\\.md$" 'markdown-mode)
                    (cons "\\.markdown$" 'markdown-mode)
                    (cons "README\\.md$" 'gfm-mode)
;		    (cons "\\.prom$" 'promela-mode)
;		    (cons "\\.hs$" 'haskell-mode)
;		    (cons "\\.sml$" 'sml-mode))
		    )
            auto-mode-alist))

(provide 'mymode)
;;; mymode.el ends here
