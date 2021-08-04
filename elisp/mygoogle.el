;;; mygoogle.el --- 

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author:  Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; Location: http://www002.upp.so-net.ne.jp/mamewo/mygoogle.el #
;; Keywords: 
;; Version: $Id: mygoogle.el 1082 2008-12-22 14:41:46Z tak $

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
;; UTFエンコードでshellを用いるのでへんなコマンドを実行しないように
;; &;<> を潰した
;; うーん、UTFエンコードするかんすうを書けばいいのだ
;; 
;(defun my-word-at-position ()
;  (interactive)
;  (if (tooltip-region-active-p)
;      (buffer-substring (region-beginning)
;			(region-end))
;    (thing-at-point 'word)))

;(require 'browse-url)

;;; Code:
(defvar my-google-history-list nil)

(defvar my-google-base 
  "http://www.google.co.jp/search?ie=UTF-8&oe=UTF-8&hl=ja&lr=")
;; EUC, JISなどをutf8 -> urlencodeし標準出力にだすプログラム
(defvar my-utf8-encode-program
  (expand-file-name "~/bin/encode_utf8.pl"))
;;; encoded-string を更新
(defun my-google-filter (process out)
  (setq encoded-string out))

;; 一語の検索
;; 一語の検索
(defun my-google-search (word &optional urlp)
  (let* ((url (if urlp
		  word
		(let* ((word-list 
			(split-string (replace-in-string word "[';\|<>]" "")))
		       (safer-word-string
			(my-fold-left 
			 '(lambda (x y) (concat  x " " y ))
			 "" word-list))
		       (encoded-word
			(shell-command-to-string
			 (format "%s %s" my-utf8-encode-program safer-word-string))))
		  (concat my-google-base "&q=" encoded-word)))))
    (browse-url url)))

;		(let* ((word-list 
;			(split-string (replace-in-string word "[';\|<>]" "")))
;		       (safer-word-string
;			(my-fold-left 
;			 '(lambda (x y) (concat  x " " y ))
;			 "" word-list))
;		       (encoded-word
;			(shell-command-to-string
;			 (format "%s %s" my-utf8-encode-program safer-word-string))))


(defun my-google-search-at-point ()
  (interactive)
  (my-google-search (my-word-at-position)))

(defun my-google-search-in-minibuffer ()
  (interactive)
  (let ((url-or-keyword 
	 (read-from-minibuffer "searching word: " 
			       "" nil nil 'my-google-history-list)))
    (my-google-search url-or-keyword
		      (string-match "^http://" url-or-keyword)))) ;長さをみるのが面倒なので

(provide 'mygoogle)

;;; mygoogle.el ends here
