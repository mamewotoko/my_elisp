;;; search_ocaml_type.el --- 
;; Author:  Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;; Keywords: 
;;; Code:

(defconst type-server-program (expand-file-name "~/bin/type_server"))
(defconst type-server-process-name "type_server")
(defconst type-server-buffer-name "*search_result*")
(defvar my-search-ocaml-process nil)
(defvar my-search-ocaml-buffer-string "")
(defvar my-ocaml-search-exact-mode nil)

(defconst my-search-ocaml-result-regexp "^__RESULT\t\\([^\n]*\\)\n")
(defconst my-search-ocaml-start-regexp "^__START\n")
(defconst my-search-ocaml-end-regexp "^__END\n")
(defconst my-search-ocaml-error-regexp "^__\\([A-Z]+\\)_ERROR\n")

(defun my-search-ocaml-filter (proc str)
  (let ((old-buffer (current-buffer)))
    (let ((continue-p t))
      (set-buffer type-server-buffer-name)
      (setq my-search-ocaml-buffer-string
	    (concat my-search-ocaml-buffer-string str))
      (while (and my-search-ocaml-buffer-string continue-p)
	(setq continue-p nil)
	(cond 
	 ((string-match my-search-ocaml-start-regexp
			my-search-ocaml-buffer-string)
		    (progn (setq continue-p t)
			   (erase-buffer)
			   (setq my-search-ocaml-buffer-string
				 (substring my-search-ocaml-buffer-string
					    (match-end 0)))))
	 ((string-match my-search-ocaml-result-regexp
			my-search-ocaml-buffer-string)
	  (let ((result (match-string 1 my-search-ocaml-buffer-string)))
	    (setq continue-p t)
	    (insert-string (concat result "\n"))
	    (setq my-search-ocaml-buffer-string
				 (substring my-search-ocaml-buffer-string
					    (match-end 0)))))
	 ((string-match my-search-ocaml-end-regexp
			my-search-ocaml-buffer-string)
	  (progn 
	    (setq continue-p nil)
	    (setq my-search-ocaml-buffer-string
		  (substring my-search-ocaml-buffer-string
			     (match-end 0)))
	    (message "search finished")))
	 (t (setq continue-p nil)))))))


(defun my-search-ocaml-start-process ()
  (setq my-search-ocaml-process
	(start-process type-server-process-name 
		       type-server-buffer-name
		       type-server-program "-I" "+labltk"))
  (set-process-filter my-search-ocaml-process 'my-search-ocaml-filter))

(defun my-search-ocaml-function (type searchtype)
  (let ((searchtypestring 
	 (if (eq searchtype 'type)
	     "SEARCHTYPE" 
	   (if (eq searchtype 'value)
	       "SEARCHVALUE"
	     "SEARCH")))
	(exact-or-not (if my-ocaml-search-exact-mode "Exact" "Included")))
    (process-send-string my-search-ocaml-process (concat searchtypestring "\t" type "\t" exact-or-not "\n"))
    (accept-process-output my-search-ocaml-process 0 1)
    (switch-to-buffer type-server-buffer-name)))

;; to test
;(my-search-ocaml-values-by-type-from-minibuffer)

(defconst my-ocaml-seach-mode-name "otype")
(defvar my-ocaml-seach-mode-map (make-keymap "my-ocaml-search-map"))
(define-key my-ocaml-seach-mode-map [return] 'my-ocaml-search-open-mli)
(defconst my-ocaml-include-path (split-string (shell-command-to-string "find `ocamlc -where` -type d") "\n"))

(defun my-get-module-name ()
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (let ((end (search-forward "." nil t)))
	(if end 
	    (let ((nameend (search-forward " " nil t)))
	      (if nameend (cons (buffer-substring start (- end 1))
				(buffer-substring end (- nameend 1))))))))))

(defun my-ocaml-search-open-mli ()
  (interactive)
  (let ((module-name (car (my-get-module-name)))
	(valuename (cdr (my-get-module-name))))
    (if module-name 
	(let* ((len (length module-name))
	       (mli-filename 
		(concat (char-to-string (downcase (aref module-name 0)))
			(substring module-name 1 len)
			".mli"))
	       (path (locate-file mli-filename my-ocaml-include-path)))
	  (if path (progn 
		     (find-file path)
		     (goto-char 1)
		     (search-forward-regexp (concat "^\\(type\\|val\\) +" valuename) nil t))
	    (error (concat "cannot find path: " path "/" mli-filename))))
      (message (concat "cannot find module-name: " module-name)))))

(defun my-ocaml-search-mode ()
  (interactive)
  (setq mode-name "otype")
  (use-local-map my-ocaml-seach-mode-map))
  
(defun my-search-ocaml-all ()
  (interactive)
  (let ((type (read-string "type/value: ")))
    (my-search-ocaml-start-process)
    (my-search-ocaml-function type 'all)
    (my-ocaml-search-mode)))

(defun my-search-ocaml-value ()
  (interactive)
  (let ((type (read-string "value: ")))
    (my-search-ocaml-start-process)
    (my-search-ocaml-function type 'value)
    (my-ocaml-search-mode)))

(defun my-search-ocaml-type ()
  (interactive)
  (let ((type (read-string "type: ")))
    (my-search-ocaml-start-process)
    (my-search-ocaml-function type 'type)
    (my-ocaml-search-mode)))

(provide 'search_ocaml_type)

;;; search_ocaml_type.el ends here
