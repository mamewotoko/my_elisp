;;; mypromelaextend.el --- 

;; Copyright (C) 2003 by Free Software Foundation, Inc.

;; FTP Directory: sources/emacs #
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
;; spin: line   1 "processes.prom", Error: syntax error	saw 'an identifier' near 'Thread1'

;;; Code:

(load "mylibrary.el")
(defconst my-promela-error-regexp "spin: line +\\([0-9]+\\) \"\\([^\"]+\\)\"")

(defun my-promela-header-insert ()
  (let ((f (file-name-nondirectory (buffer-file-name))))
    (insert-header "//" "")
    (insert-string (format "//\t\t\t%s     \n// FTP Directory: sources/promela #\n// Compile: spin -a %s && gcc pan.c -o pan #\n\n" time-stamp-start f))))

(defun my-promela-jump-to-error-point (error-message)
  (and (string-match my-promela-error-regexp error-message)
       (let ((filename (match-string 2 error-message))
	     (line-number (match-string 1 error-message)))
	 (message (format "matched as promela file: %s, line %s" filename line-number))
	 (if (file-exists-p filename)
	     (progn
	       (find-file-other-window filename)
	       (goto-line (string-to-int line-number))
	       t)
	   (progn
	     (message "Can't find: %s" filename)
	     nil)))))

(add-hook 'promela-mode-hook
	  '(font-lock-mode 1))

(provide 'mypromelaextend)

;;; mypromelaextend.el ends here
