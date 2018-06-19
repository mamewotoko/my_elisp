;;; myinsert.el --- Last modified: Tue Jun 19 21:16:51 2018
;; Location: http://www002.upp.so-net.ne.jp/mamewo/sources/emacs/myinsert.el #
;; FTP Directory: sources/emacs #
;; Author: MASUYAMA Takashi <mamewotoko@gmail.com>

;;; Code:

(require 'autoinsert)
(load "myinsertlibs.el")

(defconst sources-of-work "/home/tak/work/home/emfg/source/")
(defconst my-c-ftp-directory "sources/c")
(defconst my-line "------------------------------------------------------------")

(add-hook  'find-file-hooks 'auto-insert)

;;;各ファイル上 time-stamp-line-limit 行は触らない。
(setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y")
(setq time-stamp-start "Last modified: ")
(setq time-stamp-end "\$")
(setq time-stamp-active t)
(setq time-stamp-line-limit 5)
(add-hook 'write-file-hooks 'time-stamp)

(defun insert-c-header-if-possible ()
  (let* ((this-file-name (buffer-file-name))
	 (header-file-name
	  (concat (file-name-prefix this-file-name)
		  ".h")))
    (if (file-exists-p header-file-name)
	(insert-string
	 (concat "#include \""
		 (file-name-nondirectory header-file-name)
		 "\"\n")))))

(load "myjavamode.el")

(defun camllexer-header-function ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-header "(*" "*)")
    (insert-string (format "(*  \t\t\t%s\n * Compile: ocamllex %s #\n *)\n\n" time-stamp-start f))
    (insert-file camllexer-template-filename)
    ))

(defun camlparser-header-function ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-header "/*" "*/")
    (insert-string (format "/*  \t\t\t%s     \n * Compile: ocamlyacc %s #\n */\n\n" time-stamp-start f))
    (insert-file camlparser-template-filename)
    ))

(defvar tex-compiler "platex")

(defun tex-template-function ()
  (insert-header "%%%" "")
  (insert-string (concat "%%%\t\t\t"
			 time-stamp-start
			 "\n"))
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-string (concat "%%%  Compile: " tex-compiler " " f "#\n")))
  (insert-file
   (concat auto-insert-directory "tex-template.tex")))

(defun sh-template-function ()
  (insert-string "#! /bin/sh\n"))

(defun perl-template-function ()
  (insert-string (concat "#! /usr/bin/perl -w\n#" my-line "\n"))
  (insert-header "#" "")
  (insert-string (concat "#\t\t\t" time-stamp-start "\n#" my-line "\n# " my-signature "# FTP Directory: sources/perl #\nuse strict;\n\n")))

(defun emacs-lisp-template-function ()
  (insert-header ";;" "")
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (prefix (file-name-prefix file-name)))
    (insert-string (concat ";;\t\t\t" time-stamp-start "\n;;" my-line "\n;; " my-signature ";; FTP Directory: sources/emacs #\n;;\n\n\n(provide '" prefix ")\n" ))))

(defun cgi-template-function () (perl-template-function))

(defun opa-template-function ()
  (insert-file (concat my-auto-insert-path
		      "opa-template.opa"))
  (end-of-buffer))

(defun html-template-function ()
  (insert-file (concat my-auto-insert-path
		       "html-template.html"))
  (beginning-of-buffer)
  (replace-string "[LOCATION]"
		  (concat my-web-base "/" 
			  (file-name-nondirectory (buffer-file-name))))
  (beginning-of-buffer))

(setq auto-insert-query nil
      auto-insert-directory my-auto-insert-path
      auto-insert-alist
      (append
       '(
	 ("\\.el$" . emacs-lisp-template-function)
	 ;("\\.cc$" . c++-template-function)
	 ;("\\.c$"  . c-template-function)
	 ("\\.tex$". tex-template-function)
	 ("\\.html$" . html-template-function)
	 ("\\.opa$" . opa-template-function)
	 ("\\.sh$" . sh-template-function)
	 ("[Mm]akefile$". makefile-template-function)
	 ("Makefile\\..+$" . caml-submakefile-template-funcion)
	 ("\\.prom$" . my-promela-header-insert)
	 )
       auto-insert-alist))

(setq html-helper-build-new-buffer nil)
;; 
(provide 'myinsert)
;;; myinsert.el ends here
