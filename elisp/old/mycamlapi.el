;;; camlapi.el --- 
;;                              Last modified: Sat Jul 07 21:10:31 2018

;; Copyright (C) 2002 by Free Software Foundation, Inc.

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
; Ocamlのライブラリ検索 Webブラウザバージョン
; dot notationでフルに指定するとそれのAPIを返す
; かなり遅いと思われ...
; 完全マッチ。
; ・パスを指定すればそれを表示 Sys.argvなど
; ・モジュール名のみを指定してもよい Sysのように。そのときは
;   そのモジュールのページを表示する
; ・名前のみ指定した場合 Pervasivesモジュールを参照する
; openへの対応は......?
; ocamlbrowserは複雑そうだった。 cf) otherlibs/labltk/browser
;; 

;;; Code:
;; OcamlのHTMLバージョンのマニュアルのURL。
(defvar my-caml-library-maual-url "file:/home/tak/Doc/htmlman/libref")

(defun my-browse-caml-api-from-word (word)
  (interactive)
  (let* ((selected-string word)
	 (splitted (split-string selected-string "\\."))
	 (len (length splitted)))
    (if (>= len 2) ;;valあり
	(let* ((splitted-list (my-split-list splitted (- len 1)))
	       (module-path (my-join (car splitted-list) "."))
	       (val (car (car (cdr splitted-list)))))
	  (my-browse-caml-api module-path val))
      (let* ((element (car splitted))
	     (fst-char (aref element 0)))
	(if (and (>= fst-char ?A)
		 (<= fst-char ?Z))
	    (my-browse-caml-api element nil)
	  (my-browse-caml-api "Pervasives" element))))))

(defun my-browse-caml-api-at-position ()
  (interactive)
  (let ((word (or (my-word-at-position) (read-string "variable: "))))
    (my-browse-caml-api-from-word word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モジュールを.記法で、あとメンバをくれるとブラウザにAPIを
;; 表示する。
(defun my-browse-caml-api (module element)
  (let ((url (format "%s/%s.html%s" my-caml-library-maual-url module
		     (if element (concat "#VAL" element) ""))))
    (browse-url url)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LIST操作
;
(defun my-split-list (lst pos)
    (if (= pos 0)
	(list nil lst)
      (let ((res (my-split-list (cdr lst) (- pos 1))))
	(list (cons (car lst) (car res)) (car (cdr res))))))

(defun my-join (string-lst delim)
  (if string-lst
      (let ((res (my-join (cdr string-lst) delim)))
	(concat (car string-lst) (if res (concat delim res) "")))
    nil))

(provide 'mycamlapi)

;;; camlapi.el ends here
