;;; mytexextend.el --- 

;; Copyright (C) 2002 by Free Software Foundation, Inc.

;; Author: Takashi Masuyama <tak@yl.is.s.u-tokyo.ac.jp>
;; Keywords: platex
;; Location: http://www002.upp.so-net.ne.jp/mamewo/mytexextend.el #

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
;; texのエラーもめんどっち。ので飛びます。
;; メモ
;;   エラー出力の書式
;;   エラーが出たときにはカーソル行の先頭に?が出る。
;;   その数行上にエラー箇所を表す文字がある。 l.[line_number]
;;   エラーがでたファイル名はずっと上にある。アドホックに.texを探す。(汗)
;;   ** 多分\input には対応しない。するときは作者が困ったとき

;;; Code:

;;手続き的で副作用あり。

(defun my-platex-get-error-line ()
  (search-backward-regexp "^l.\\([0-9]+\\) ")
  (buffer-substring (match-beginning 1)
		    (match-end 1)))

(defun my-platex-get-error-filename ()
  (search-backward-regexp "^(\\([a-zA-Z0-9._-]+.tex\\)$")
  (buffer-substring (match-beginning 1)
		    (match-end 1)))

;; カーソル位置は ? の行ね
;;
(defun my-platex-jump-to-error (error-message)
  (and (eq (aref error-message 0) ??)
      (let ((initial-point (point)) ;; initial-point へは goto-charで飛べる
	    ;; 呼び出し順序が重要 (汗)
	    (line-number (my-platex-get-error-line))
	    (filename (my-platex-get-error-filename)))
	(goto-char initial-point)
	(my-error-jump-to-point filename line-number))))

(provide 'mytexextend)

;;; mytexextend.el ends here
