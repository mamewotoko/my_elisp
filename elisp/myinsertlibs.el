;;; myinsertlibs.el --- 

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; Author:  <mamewo@dk9.so-net.ne.jp>
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

(defvar my-line
  "------------------------------------------------------------")
(defvar my-perl-path "/usr/bin/perl")
(defvar my-auto-insert-path "~/lib/emacs/template/")
;(defvar my-web-base "http://www10.u-page.so-net.ne.jp/dk9/mamewo")
(defvar my-web-base "http://www002.upp.so-net.ne.jp/mamewo")
(defvar my-full-name "Takashi Masuyama")
(defvar my-mail-address "mamewo@dk9.so-net.ne.jp")
(defvar my-signature (format "Written by %s <%s>\n" my-full-name my-mail-address))

(defun insert-header (start-comment end-comment)
  (let ((this-file-name (file-name-nondirectory
			  (buffer-file-name))))
    (insert-string (concat start-comment
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

(provide 'myinsertlibs)

;;; myinsertlibs.el ends here
