;;; myinsert.el --- Last modified: Thu Jun 25 07:39:55 2020
;; Author: MASUYAMA Takashi <mamewotoko@gmail.com>

;;; Code:

(require 'autoinsert)

(defconst my-c-ftp-directory "sources/c")
(defconst my-line "------------------------------------------------------------")

(defvar my-perl-path "/usr/bin/perl")
(defvar my-auto-insert-path "~/lib/emacs/template/")
(defvar my-web-base "https://mamewo.ddo.jp/")
(defvar my-full-name "Takashi Masuyama")
(defvar my-mail-address "mamewo@dk9.so-net.ne.jp")
(defvar my-signature (format "Written by %s <%s>\n" my-full-name my-mail-address))

(defun insert-header (start-comment end-comment)
  (let ((this-file-name (file-name-nondirectory
			  (buffer-file-name))))
    (insert (concat start-comment
			   " "
			   this-file-name
			   (let ((file-name-length (length this-file-name)))
			     (if (> file-name-length 12)
				 "\t"
			       (if (> file-name-length 5)
				   "\t\t" "\t\t\t")))
			   "Created      : "
			   (current-time-string)
			   end-comment
			   "\n"))))

(add-hook  'find-file-hooks 'auto-insert)

;;;各ファイル上 time-stamp-line-limit 行は触らない。
(setq time-stamp-format "%3a %3b %02d %02H:%02M:%02S %:y")
(setq time-stamp-start "Last modified: ")
(setq time-stamp-end "\$")
(setq time-stamp-active t)
(setq time-stamp-line-limit 5)
(add-hook 'write-file-hooks 'time-stamp)

;(load "myjavamode.el")

(defun camllexer-header-function ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-header "(*" "*)")
    (insert (format "(*  \t\t\t%s\n * Compile: ocamllex %s #\n *)\n\n" time-stamp-start f))
    (insert-file-contents camllexer-template-filename)
    ))

(defun camlparser-header-function ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-header "/*" "*/")
    (insert (format "/*  \t\t\t%s     \n * Compile: ocamlyacc %s #\n */\n\n" time-stamp-start f))
    (insert-file-contents camlparser-template-filename)
    ))

(defvar tex-compiler "platex")

(defun tex-template-function ()
  (insert-header "%%%" "")
  (insert (concat "%%%\t\t\t"
			 time-stamp-start
			 "\n"))
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert (concat "%%%  Compile: " tex-compiler " " f "#\n")))
  (insert-file-contents
   (concat auto-insert-directory "tex-template.tex")))

(defun sh-template-function ()
  (insert "#! /bin/sh\n"))

(defun perl-template-function ()
  (insert (concat "#! /usr/bin/perl -w\n#" my-line "\n"))
  (insert-header "#" "")
  (insert (concat "#\t\t\t" time-stamp-start "\n#" my-line "\n# " my-signature "# FTP Directory: sources/perl #\nuse strict;\n\n")))

(defun cgi-template-function () (perl-template-function))

(defun opa-template-function ()
  (insert-file-contents (concat my-auto-insert-path
		                "opa-template.opa"))
  (goto-char (point-max)))

(defun html-template-function ()
  (insert-file-contents (concat my-auto-insert-path
                                "html-template.html"))
  (goto-char (point-min))
  (replace-string "[LOCATION]"
		  (concat my-web-base "/" 
			  (file-name-nondirectory (buffer-file-name))))
  (goto-char (point-min)))

(defun markdeep-template-function ()
  (insert-file-contents (concat my-auto-insert-path
                                "markdeep.md")))

(setq auto-insert-query nil
      auto-insert-directory my-auto-insert-path
      auto-insert-alist
      (append
       '(
         ("\\.tex$". tex-template-function)
         ("\\.md.html$" . markdeep-template-function)
         ("\\.html$" . html-template-function)
         ("\\.opa$" . opa-template-function)
         ("\\.sh$" . sh-template-function)
         ;; ("[Mm]akefile$". makefile-template-function)
         ;; ("Makefile\\..+$" . caml-submakefile-template-funcion)
         ("\\.prom$" . my-promela-header-insert)
         )
       auto-insert-alist))

(setq html-helper-build-new-buffer nil)
;; 
(provide 'myinsert)
;;; myinsert.el ends here
