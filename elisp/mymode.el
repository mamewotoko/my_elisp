;;; mymode.el --- Last modified: Sat Jul 07 20:49:10 2018
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
(define-key Info-mode-map "\M-n" 'my-Info-search-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML mode
;;
;;

(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")

(custom-set-variables
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120" "--ignore=E128,D103,E501,D100,D103"))))

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

(setq-default c-basic-offset 4)

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

(defun my-hadolint-jump-to-error-point (error-message)
  (and (string-match "^\\([^\"]+\\):\\([0-9]+\\) \\(.*\\)" error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message)))
	 (my-error-jump-to-point filename line-number 0))))

(defun my-xvcg-jump-to-error (error-message)
  (and (string-match "^Syntax error (\\([^:]+\\): l:\\([0-9]+\\)" error-message)
       (let ((filename (match-string 1 error-message))
	     (linenumber (match-string 2 error-message)))
	 (my-error-jump-to-point filename linenumber))))

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
       (my-hadolint-jump-to-error-point error-message)
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

(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")

(custom-set-variables
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120"))))

(add-hook 'python-mode-hook
          '(lambda ()
             (flymake-mode t)
             (flymake-python-pyflakes-load)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perl mode
;;
;;
;(load "myperlextend.el")

;; (add-hook 'cperl-mode-hook
;; 	  '(lambda ()
;; 	     (define-key cperl-mode-map
;; 	       "\C-h"
;; 	       'backward-delete-char)
;; 	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDE mode
;;
;;

(add-hook 'java-mode-hook
	  '(lambda ()
	     (setq tab-width 4)))

(add-hook 'calendar-mode-hook
	  '(lambda ()
	     (define-key calendar-mode-map
	       "\C-v" (other-window 1))))

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

;;; tags
;;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter               
;;;  find-tag that automagically reruns etags when it cant find a               
;;;  requested item and then makes a new try to locate it.                      
;;;  Fri Mar 15 09:52:14 2002    
(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

(provide 'mymode)
;;; mymode.el ends here
