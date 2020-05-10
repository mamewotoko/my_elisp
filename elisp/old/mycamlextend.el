;;; mycamlextend.el --- 
;;; FTP Directory: sources/emacs #
;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
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
(load "myerrorjump.el")

(defconst my-caml-error-regexp
  "File \"\\([^\"]\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):")

(defconst my-ocamlyacc-error-regexp
  "line \\([0-9]+\\) of \"\\([^\"]+\\)\"")

(defconst my-ocamllex-error-regexp
  "File \"\\([^\"]+\\)\", line \\([0-9]+\\), character \\([0-9]+\\):")

(defconst my-ocamlrunstacktrace-jump-to-error-regexp
  "file \"\\([^\"]+\\)\", line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\)")

(defun my-ocamlrunstacktrace-jump-to-error-at-point-sub (error-message &optional print-p)
  (and (string-match my-ocamlrunstacktrace-jump-to-error-regexp error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message))
	     (column (match-string 3 error-message)))
	 (my-error-jump-to-point filename line-number column))))

(defun my-ocamlyacc-jump-to-error-at-point-sub (error-message &optional print-p)
  (and (string-match my-ocamlyacc-error-regexp error-message)
       (let ((filename (match-string 2 error-message))
	     (line-number (match-string 1 error-message)))
	 (my-error-jump-to-point filename line-number))))

(defun my-ocamllex-jump-to-error-at-point-sub (error-message &optional print-p)
  (and (string-match my-ocamllex-error-regexp error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message))
	     (charactor (match-string 3 error-message)))
	 (my-error-jump-to-point filename line-number charactor))))

(defun my-caml-jump-to-error-at-point-sub (error-message &optional print-p)
  (and (string-match  my-caml-error-regexp error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message))
	     (charactor (match-string 3 error-message)))
	 (my-error-jump-to-point filename line-number charactor))))
	 
(defun my-caml-jump-to-error-at-point-entry-point (error-message &optional print-p)
  (or (my-caml-jump-to-error-at-point-sub error-message print-p)
      (my-ocamlrunstacktrace-jump-to-error-at-point-sub error-message print-p)
      (my-ocamlyacc-jump-to-error-at-point-sub error-message print-p)
      (my-ocamllex-jump-to-error-at-point-sub error-message print-p)))

(provide 'mycamlextend)

;;; mycamlextend.el ends here