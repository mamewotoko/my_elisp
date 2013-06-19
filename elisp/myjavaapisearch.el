;;; myjavaapisearch.el --- 
;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

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
;;compile
; (setq load-path (cons (expand-file-name "~/lib/emacs/lisp/namazu") load-path))
;
(defconst my-java-search-base-path "/home/tak/Doc/docs/api/")
(defconst my-java-search-path-string "/home/tak/Doc/docs/api/java")
(defconst my-java-browse-protocol "file://")
(defconst my-java-api-search-output-buffer "*API Search Result*")
(defconst my-java-namazu-index (expand-file-name "~/.java_namazu"))
(defconst my-java-namazu-from-source-index (expand-file-name "~/.java_namazu/from_source"))
(defconst my-java-namazu-index-for-servlet (expand-file-name "~/Doc/servlet_namazu"))

(defun my-java-search-api (target exactp)
  (let* ((target-filename
	  (if exactp (concat target ".html")
	    (format "\"*%s*.html\"" target)))
	 (buf
	  (get-buffer-create
	   my-java-api-search-output-buffer)))
    (insert-string 
     (format "<html><head><title>%sの検索結果</title></head><meta http-equiv=\"Content-Type\" content=\"text/html; charset=euc-jp\"><body bgcolor=#ffffff><center><u>%sの検索結果</u></center><blockquote>%s</blockquote></body></html>"
	     target target
	     (shell-command-to-string 
	      (format
	       "find %s -name %s -printf '<a href=\"%s%%p\">%%P</a><br>\\n\'"
	       my-java-search-path-string target-filename my-java-browse-protocol)))
     buf)
    (browse-url-of-buffer buf)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive
(defun my-java-exact-search-api ()
  (interactive)
  (let ((target (my-word-at-position)))
    (my-java-search-api target t)))

(defun my-java-apropos-api ()
  (interactive)
  (let ((target (my-word-at-position)))
    (my-java-search-api target nil)))

;(defun my-java-grep-api ()
;  (interactive)
;  (let ((target (my-word-at-position)))
;    (my-java-grep target)))

(load "mylibrary.el")
(defun my-java-keyword-search ()
  (interactive)
  (let ((word (read-string "Keyword: ")))
    (namazu 0 my-java-namazu-index word)))

(defun my-java-keyword-search-for-servlet ()
  (interactive)
  (let ((word (read-string "Keyword: ")))
    (namazu 0 my-java-namazu-index-for-servlet word)))

(defun my-java-search-keyword-at-point ()
  (interactive)
  (let ((word (thing-at-point 'word)))
    (namazu 0 my-java-namazu-index word)))

(defun my-java-search-keyword-from-source ()
  (interactive)
  (let ((word (my-word-at-position)))
    (namazu 0 my-java-namazu-from-source-index word)))

(provide 'myjavaapisearch)

;;; myjavaapisearch.el ends here
