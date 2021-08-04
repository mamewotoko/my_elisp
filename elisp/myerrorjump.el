;;; myerrorjump.el --- 

;; Copyright (C) 2003 by Free Software Foundation, Inc.
;; FTP Directory: sources/emacs #
;; Author:  Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
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

(defun my-error-jump-to-point (filename line-number &optional charactor)
  (if (file-exists-p filename)
      (progn
	(find-file-other-window filename)
	(goto-line (string-to-number line-number))
	(forward-char (if charactor (string-to-number charactor) 0))
	(message "done (file %s | line %s | char %s)" 
		 filename line-number charactor)
	t)
    (progn (message "file %s is not found" filename) t)))

(provide 'myerrorjump)

;;; myerrorjump.el ends here
