;;; myocamlmakefile.el --- 

;; Copyright (C) 2001 by Free Software Foundation, Inc.

;; Author: Takashi Masuyama <tak@localhost.localdomain>
;; Keywords: 

;; This file is part of XEmacs.

;; XEmacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; XEmacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Synched up with: Not in FSF

;;; Commentary:
;; 

(load "myinsertlibs.el")

(defvar ocamlmakefile-path
  (expand-file-name "~/lib/ocaml/OcamlMakefile"))

(defun caml-make-ocamlmakefile (&optional source)
  (interactive)
  (let ((makefile-filename "Makefile"))
    (if (not (file-exists-p makefile-filename))
	(let* ((buf (create-file-buffer makefile-filename))
	       (s (if source source ""))
	       (prefix (if source (file-name-prefix source) "")))
	  (switch-to-buffer buf)
	  (insert
	   (format "## Makefile\n## for OcamlMakefile\n## Compile: make #\nSOURCES = %s\nRESULT = %s\nOCAMLOPT = ocamlopt.opt\nall: native-code\ninclude $(OCAMLMAKEFILE)\n" s prefix))
	  (write-file makefile-filename)
	  (kill-buffer buf)
	  ))))

(defun caml-header-function ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (caml-make-ocamlmakefile f)
    (insert
     "(************************************************************\n")
    (insert-header "  " "")
    (insert (format "  \t\t\t%s\n  Compile: make #\n" time-stamp-start))
    (insert
     "************************************************************)\n")))

;;; Code:
(provide 'myocamlmakefile)

;;; myocamlmakefile.el ends here
