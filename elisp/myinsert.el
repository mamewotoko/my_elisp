;;; myinsert.el --- Last modified: Thu May 05 10:56:16 2016
;; Location: http://www002.upp.so-net.ne.jp/mamewo/sources/emacs/myinsert.el #
;; FTP Directory: sources/emacs #
;; Author: MASUYAMA Takashi <mamewotoko@gmail.com>

;;; Code:

(require 'autoinsert)
(load "myinsertlibs.el")
(load "mycompile.el")

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

(defun c++-template-function ()
  (insert-header "//" "")
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (prefix (file-name-prefix file-name)))
    (insert-string 
     (format "//\t\t\t%s\n// %s g++ -g %s -o %s %s\n// %s ./%s %s\n//%s\n//\n"
	     time-stamp-start
	     mycompile-start
	     file-name
	     prefix
	     mycompile-end
	     myexecute-start
	     prefix
	     mycompile-end
	     my-line
	   ))
    (insert-c-header-if-possible)
    (insert-file
     (concat auto-insert-directory "c++-template.cc"))))

(load "myjavamode.el")

(defun c-template-function ()
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
	 (prefix (file-name-prefix filename))
	 (ch (progn (message "g)tk or not")
		    (read-char)))
	 (gtk-q (eq ch ?g))
	 (compile-command
	  (if gtk-q (format "gcc %s -o %s `pkg-config --cflags --libs gtk+-2.0`"
			    filename prefix)
	    (format "gcc %s -o %s" filename prefix))))
      (progn (insert-header "/*" " */")
	     (insert-string 
	      (format "/*\t\t\t%s\n * %s %s #\n * %s ./%s %s\n *%s\n *\n *\n *\n */\n"
		      time-stamp-start
		      mycompile-start
		      compile-command
		      myexecute-start
		      prefix
		      mycompile-end
		      my-line))
	     (insert-c-header-if-possible)
	     (insert-file
	      (concat auto-insert-directory 
		      (if gtk-q "gtk_template.c" "c-template.c"))))))


(defun c-header-function ()
  (let* ((this-file-name (file-name-nondirectory (buffer-file-name)))
	 (def-string (upcase this-file-name)))
    (let ((counter 0))
      (insert-header "/*" " */")
      (while (not (eq (aref def-string counter) ?.))
	(setq counter (+ counter 1)))
      (aset def-string counter ?_))
    (insert-string
     (format "/*\t\t\t%s\n *%s\n *\n *\n */\n#ifndef %s\n#define %s\n\n\n\n#endif"
	     time-stamp-start
	     my-line
	     def-string
	     def-string))
    (search-backward "#define")
    (next-line 2)))

(defun caml-header-function ()
  (let* ((f (file-name-nondirectory (buffer-file-name)))
	 (ch (progn (message "camlT)k or LablG)tk2 or not") (read-char)))
	 (camltk-q (eq ch ?t))
	 (lablgtk-q (eq ch ?g)))
    (insert-string
     "(************************************************************\n")
    (insert-header "  " "")
    (insert-string (format "  \t\t\t%s\n"
			   time-stamp-start (file-name-prefix f)))
    (insert-string
     "************************************************************)\n(**\n\n  @author Takashi Masuyama <mamewotoko@gmail.com>\n\n*)\n")
    (if camltk-q (insert-string "\nopen Tk\n\nlet _ =\n  let window = openTk () in\n  mainLoop ()\n") (if lablgtk-q (insert-string "open GtkWindow\nopen Gobject\nopen Gobject.Data\n\nlet _ =\n  let window = Window.create ~kind:`TOPLEVEL [] in\n(*  GtkBase.Container.add window something; *)\n\n  GtkBase.Widget.show_all window;\n  GtkMain.Main.main ()\n;;")))))

(defvar camllexer-template-filename (expand-file-name "~/lib/emacs/template/lexer-template.mll"))
(defvar camlparser-template-filename (expand-file-name "~/lib/emacs/template/parser-template.mly"))

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

(defun makefile-template-function ()
  (let* ((ch (progn (message "c)aml or j)ava or not")
		     (read-char)))
	  (caml-q (eq ch ?c))
	  (java-q (eq ch ?j)))
       (insert-header "###" "")
       (insert-string (format "###\t\t\t%s\n" time-stamp-start))
       (if caml-q (insert-string "CODE=nc\nTARGET=\n\n.PHONY: all clean release $(TARGET)\n\nall: $(TARGET)\n\n$(TARGET):\n\tmake -f Makefile.$@ $(CODE)\n\nrelease:\n\tmake clean\n\t( DIRNAME=`pwd | perl -pe 's|^.*/(.*)$$|$$1|'`; \\\n\tcd ..; \\\n\tfind $$DIRNAME -path '*.svn' -prune -o -path $$DIRNAME -o -print \\\n\t| xargs tar cfvz $$DIRNAME.tar.gz )\n\nclean:\n\tfor target in $(TARGET); do \\\n\t\tmake -f Makefile.$$target clean; \\\n\tdone\n\trm -f *.annot *~\n")
	 (if java-q (insert-string "ARCHIVE=\nCLASSES=classes\nMANIFEST=\nSOURCES=\nSOURCE_FILES=$(SOURCES) $(MANIFEST)\n\n$(CLASSES)/$(ARCHIVE): $(SOURCE_FILES)\n\t[ -e $(CLASSES) ] || mkdir $(CLASSES)\n\trm -f $@\n\t$(JAVAC) $(CLASSES) $(SOURCES)\n\tcp $(SOURCE_FILES) $(CLASSES)\n\t(cd $(CLASSES) ; \\\n\tif [ -z \"$(MANIFEST)\" ]; then \\\n\t\tjar cvf $(ARCHIVE) * ; \\\n\t\telse\\\n\t\tjar cvmf $(MANIFEST) $(ARCHIVE) * ; \\\n\tfi )\n\nclean:\n\trm -rf $(CLASSES)")))))

(defun caml-submakefile-template-funcion ()
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
	 (source-filename (car (cdr (split-string filename "[.]")))))
    (insert-string (format "### %s\n### %s make -f %s %s\nOCAMLMAKEFILE=%s\nSOURCES=%s\nLIBS=\nINCDIRS=\nRESULT=%s\n\ninclude $(OCAMLMAKEFILE)\n" filename myexecute-start filename mycompile-end "/home/tak/lib/ocaml/OCamlMakefile" (concat source-filename ".ml") source-filename))))

(defun sh-template-function ()
  (insert-string "#! /bin/sh\n")
  (insert-header "#" "")
  (insert-string (concat "#\t\t\t" time-stamp-start "\n############################################################\n# Written by Takashi Masuyama <mamewotoko@gmail.com>\n\n")))

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

(defun haskell-template-function ()
  (insert-header "--" "")
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (prefix (file-name-prefix file-name)))
    (insert-string
     (format "--\t\t\t%s\n-- %s ghc %s -o %s %s\n-- FTP Directory: sources/haskell %s\n\n"
	     time-stamp-start
	     mycompile-start
	     file-name
	     prefix
	     mycompile-end
	     mycompile-end))))

(load "mypromelaextend.el")

(setq auto-insert-query nil
      auto-insert-directory my-auto-insert-path
      auto-insert-alist
      (append
       '(("\\.java$" . java-template-function)
	 ("\\.el$" . emacs-lisp-template-function)
	 ("\\.cc$" . c++-template-function)
	 ("\\.c$"  . c-template-function)
	 ("\\.h$"  . c-header-function)
	 ("\\.tex$". tex-template-function)
	 ("\\.html$" . html-template-function)
	 ("\\.opa$" . opa-template-function)
	 ("\\.sh$" . sh-template-function)
	 ("[Mm]akefile$". makefile-template-function)
	 ("Makefile\\..+$" . caml-submakefile-template-funcion)
	 ("\\.cgi$" . cgi-template-function)
	 ("\\.pl$" . perl-template-function)
	 ;("\\.mli?$" . caml-header-function)
	 ;("\\.mll$". camllexer-header-function)
	 ;("\\.mly$" . camlparser-header-function)
	 ("\\.prom$" . my-promela-header-insert)
	 ("\\.hs$". haskell-template-function)
	 )
       auto-insert-alist))

(setq html-helper-build-new-buffer nil)
;; 
(provide 'myinsert)
;;; myinsert.el ends here
