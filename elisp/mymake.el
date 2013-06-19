;;; mymake.el --- 

;; Copyright (C) 2001 by Free Software Foundation, Inc.

;; Author: MASUYAMA Takashi <tak@is.s.u-tokyo.ac.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

(load "mycompile.el")

(defvar my-make-make-program "make")

(defun my-make-target-backward ()
  (interactive)
  (let ((init-position (point)))
    (if (re-search-backward "^[a-zA-Z0-9%._]*:")
	(let ((end-point (- (point) 1)))
	  (beginning-of-line)
	  (let* ((start-point (point))
		 (target-name (buffer-substring start-point end-point)))
	    (shell-command
	     (format "%s %s"
		     my-make-make-program
		     target-name)
	     my-compile-buffer-name)
	    (message (format "%s have made" target-name)))))
    (goto-char init-position)))
  
;;; Code:



;;; mymake.el ends here