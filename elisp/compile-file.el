;;; compile-file.el --- 

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author:  <tak@localhost.localdomain>
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

;;; Code:
(setq load-path
      (append (list
	       (expand-file-name "~/lib/emacs/lisp")
	       (expand-file-name "~/lib/emacs/lisp/namazu")
	       (expand-file-name "~/lib/emacs/lisp/irchat")
	       "/usr/local/share/emacs/site-lisp/mew"
	       "/usr/local/lib/xemacs/mule-packages/lisp/mule-base/"
	       "/usr/local/lib/xemacs/site-lisp/yatex"
	       "/usr/local/lib/xemacs/site-lisp/ocaml"
	       "/usr/lib/xemacs/site-lisp"
	       "/usr/lib/xemacs/site-lisp/irchat-pj"
	       "/usr/local/share/emacs/site-lisp/sml-mode"
	       "/usr/lib/xemacs/site-lisp/w3m/"
	       "/usr/local/lib/xemacs/")
	      load-path))

(require 'japan-util)
(provide 'compile-file)

;;; compile-file.el ends here
