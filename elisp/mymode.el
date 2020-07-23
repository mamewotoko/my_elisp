;;; mymode.el --- Last modified: Thu Jul 23 14:37:04 2020
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

;; 2003/ 2/ 5 gdb のエラージャンプを追加。エラージャンプを大幅改造
;;   myerrorjump.el にジャンプルーチンを入れた。

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Info
(defun my-Info-search-next ()
  (interactive)
  (Info-search Info-last-search))
(require 'info)
(define-key Info-mode-map "\M-n" 'my-Info-search-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ mode
(setq-default c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp mode
;;
(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (define-key emacs-lisp-mode-map "\C-j" 'eval-print-last-sexp)
))

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
(setq auto-mode-alist
          (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

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

(require 'shell)
(add-hook 'shell-mode-hook
	  '(lambda ()
         (define-key shell-mode-map [(control return)] 'dirs)
         (define-key shell-mode-map (kbd "C-l") '(lambda ()
                                                   (interactive)
                                                   (zap-up-to-char -1 ?/)))
	     (define-key shell-mode-map
	       "\C-c\C-j" 'my-goto-error)
	     (shell-dirtrack-mode 1)
             (setq dirtrack-list '(":*\\([A-Za-z]*:*~*[\/\\].*?\\)[^-+A-Za-z0-9_.()//\\ ]" 1)) ;for help making this regular expression you may want to use "M-x re-builder", where M is usually alt
	     ))

;; ruby
(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq flycheck-checker 'ruby-rubocop)
             (flycheck-mode 1)))

;; ssh config
;(autoload 'ssh-config-mode "ssh-config-mode" t)
;(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
;(add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
;; (add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
;; (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
;(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JDE mode

(add-hook 'java-mode-hook
	  '(lambda ()
	     (setq tab-width 4)))

(add-hook 'calendar-mode-hook
	  '(lambda ()
	     (define-key calendar-mode-map
	       "\C-v" (other-window 1))))

(require 'tuareg)
;(require 'lsp)
(require 'merlin)

(add-hook 'tuareg-mode-hook #'merlin-mode)
(setq merlin-ac-setup 'easy)
(add-hook 'merlin-mode-hook
          '(lambda ()
             (define-key merlin-mode-map
               "\C-ci" 'merlin-document)))

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)


;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; Enable nice rendering of diagnostics like compile errors.
(use-package flycheck
  :init (global-flycheck-mode))

(require 'csv-mode)
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
            (cons "\\.csv$" 'csv-mode)
		    (cons "\\.java$" 'java-mode)
                    (cons "\\.md.html$" 'markdown-mode)
                    (cons "\\.md$" 'markdown-mode)
                    (cons "\\.markdown$" 'markdown-mode)
                    (cons "README\\.md$" 'gfm-mode)
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

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(provide 'mymode)
;;; mymode.el ends here
