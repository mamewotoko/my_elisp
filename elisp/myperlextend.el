;;; myperlextend.el --- 
;; FTP Directory: sources/emacs #
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
(load "myerrorjump.el")
(defun my-perl-jump-to-error-point (error-message)
  (and (string-match "at \\([a-zA-Z0-9-_./]*\\) line \\([0-9]*\\)" error-message)
       (let ((filename (match-string 1 error-message))
	     (line-number (match-string 2 error-message)))
	 (my-error-jump-to-point filename line-number))))

;;; Code:


(provide 'myperlextend)

;;; myperlextend.el ends here
