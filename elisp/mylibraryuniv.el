;;; mylibrary.el --- personal use    Last modified: Wed Jun 19 20:58:25 2013
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
; manual-entry-at-position
; apropos-at-position
; 
;; 

;;; Code:
(defvar my-path
  (append (list (expand-file-name "~/lib/emacs/lisp")
		"/usr/include"
		"/usr/include/sys/"
		"/usr/local/include"
		"/usr/local/lib/gcc-lib/sparc-sun-solaris2.6/2.95.1/include/g++/")
	  load-path))

(defun file-name-prefix (filename)
  (let ((dot-position (posix-string-match "\\." filename nil)))
    (substring filename 0 dot-position)))

(defun file-name-suffix (filename)
  (let* ((dot-position (posix-string-match "\\." filename nil)))
    (substring filename (+ dot-position 1) nil)))

;; 優先順位
;;  リージョン -> point
(defun word-at-position ()
  (if (tooltip-region-active-p)
      (buffer-substring (region-beginning)
			(region-end))
    (thing-at-point 'word)))

(defun manual-entry-at-position ()
  (interactive)
  (let ((obj (word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
      (manual-entry obj))))

;; 優先順位
;;  リージョン -> point
(defun apropos-at-position ()
  (interactive)
  (let ((obj (word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
    (apropos obj))))

;;パスによりヒントをつけることを考えても良い。面倒。
(defun my-find-library-path (filename path-list)
  (let ((path (locate-library filename t path-list)))
    (if (null path)
	;; try
	(progn (setq path (locate-library (concat filename ".el") path-list))
	       (if (null path-list)
		   (error "Not found the file! %s" filename)
		 (find-file-other-window path)))
      (find-file-other-window path))))

(defun eval-region-and-send-message ()
  (interactive)
  (eval-region (region-beginning)
	       (region-end))
  (message "** eval-region completed"))

(defun eval-buffer-and-send-message ()
  (interactive)
  (eval-buffer)
  (message "** eval-buffer completed"))

(defun compose-mail-at-position ()
  (interactive)
  (let* ((get-address (thing-at-point 'url))
	 (address-length (length get-address))
	 (real-address (substring get-address 7 address-length)))
    (compose-mail real-address)
    (forward-char (- address-length 7))))

(defvar start-process-at-region-buffer-name "*PROCESS*")

(defun start-command (command-name)
  (let ((target-buffer
	 (get-buffer-create start-process-at-region-buffer-name))
	(time (time-stamp-hh:mm:ss)))
    (insert-string (concat time " ******************************\n" command-name "\n")
		   target-buffer)
    (start-process-shell-command "process" target-buffer command-name)
    (display-buffer target-buffer)))

;;;;;;;;;List操作 基礎関数
;;連想配列のサーチ
(defun my-search-alist (key alist)
  (while (and (not (null alist))
	      (not (eq key (caar alist))))
    (setq alist (cdr alist)))
  (if (null alist)
      nil
    (cadar alist)))

(defun my-search-alist2 (key alist equality)
  "キーを用いて連想リストをサーチ。ヒットしたら連想値を返す"
  (while (and (not (null alist))
	      (not (eval (list equality key (caar alist)))))
    (setq alist (cdr alist)))
  (if (null alist)
      nil
    (cdar alist)))

(defun add-to-alist (key value alist)
  (let ((new-element (list 'cons key value)))
    (eval (list 'setq alist (list 'cons new-element alist)))))

(defvar user-url "http://www10.u-page.so-net.ne.jp/dk9/mamewo")
(defvar signature-file (expand-file-name "~/.signature"))

;;------------------------------------------------------------
;; 文字列の頭のスペースタブを取り除く。
;; それなりに速くうごくはず。資源も少ないはず。
(defun my-delete-prefix-space (string)
  (let ((count 0)
	(len (length string))
	element)
    (while (and (progn 
		  (setq element (aref string count))
		  (or (eq element ? )
		      (eq element ?\t)))
		(< count len))
      (setq count (+ count 1)))
    (substring string count)))

(defun my-delete-suffix-space (string)
  (let ((count (- (length string) 1))
	element)
    (while (and (progn 
		  (setq element (aref string count))
		  (or (eq element ? )
		      (eq element ?\t)))
		(>= count 0))
      (setq count (- count 1)))
    (substring string 0 (+ count 1))))

(defun insert-file-name ()
  (interactive)
  (insert-string (file-name-nondirectory (buffer-file-name))))

(defun insert-file-name-prefix ()
  (interactive)
  (insert-string
   (file-name-prefix (file-name-nondirectory (buffer-file-name)))))

(require 'time-stamp)
(defun insert-date-yy/mm/dd-format ()
  (interactive)
  (let ((time-string (time-stamp-yyyy/mm/dd)))
    (if (eq (aref time-string 5) ?0)
	(aset time-string 5 ?  ))
    (if (eq (aref time-string 8) ?0)
	(aset time-string 8 ?  ))
    (insert-string time-string)))

;;--------------------------------------------------
;; 個人情報などグローバルなアイテム
;;
(defun my-insert-item-menu (&optional index)
  (interactive "P")
  (message "N)ame D)ate U)rl M)ail F)ilename P)refix T)ime-mark C)ompile L)ine")
  (let ((sw (selected-window))
	(local-index (or index (read-char))))
    (message nil)
    (cond
     ((= local-index ?n) (insert-string (user-full-name)))
     ((= local-index ?d) (insert-date-yy/mm/dd-format))
     ((= local-index ?u) (insert-string user-url))
     ((= local-index ?m) (insert-string (user-mail-address)))
;     ((= local-index ?s) (insert-file signature-file))
     ((= local-index ?f) (insert-file-name))
     ((= local-index ?p) (insert-file-name-prefix))
     ((= local-index ?t) (insert-string time-stamp-start))
     ((= local-index ?l) (insert-string my-line))
     ;;; mycompile.el
     ((= local-index ?c)
      (progn 
	(insert-string (format "%s %s" mycompile-start mycompile-end))
	(backward-char (+ (length mycompile-end) 1))))
)))

(defun my-search-forward-word-at-position ()
  (interactive)
  (search-forward (word-at-position)))

(defun my-search-backward-word-at-position ()
  (interactive)
  (search-backward (word-at-position)))

;;------------------------------------------------------------
;;フルパスをguessがいいかなぁ
;;
(defun find-include-etc-at-position ()
  (interactive)
  (let ((filename 
	 (if (tooltip-region-active-p)
	     (buffer-substring (region-beginning) (region-end))
	   (let ((thing (thing-at-point 'filename)))
	     (if (equal thing "")
	       (read-string "library name: ")
	       thing)))))
    (if (not (equal filename ""))
	(let ((expanded-path (cons (expand-file-name "./")
				   my-path)))
	  (my-find-library-path filename expanded-path)))))

(defun my-measure-length-of-region ()
  (interactive)
  (if (tooltip-region-active-p)
      (message "length: %d" (abs (- (region-end) (region-beginning))))))

(defun my-copy-line ()
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (copy-region-as-kill start (point)))
  (message "copied"))

(defun my-activate-line ()
  (interactive)
  (end-of-line)
  (let ((count (point)))
    (beginning-of-line)
    (setq count (- count (point)))
    (forward-char-mark count)))

;;------------------------------------------------------------
;;start endは指定するなら一度に
;;端はソートされていなくて良い
(defun my-regional-insert (start-string end-string &optional start end)
"start endは指定するなら一度に
位置は戻らない。"
  (if (not start)
      (progn
	(if (tooltip-region-active-p)
	    (progn 
	      (setq start (region-beginning))
	      (setq end (region-end))))))
  (goto-char end)
  (insert-string end-string)
  (goto-char start)
  (insert-string start-string))

(defun my-line-length ()
  (let (start)
    (beginning-of-line)
    (start (point))
    (end-of-line)
    (+ 1 (- (point) start))))

; (defun my-push-point ()
;   (interactive)
;   (push-mark)
;   (message "Mark Pushed"))

; (defun my-pop-point ()
;   (interactive)
;   (goto-char (pop-mark)))
;;; mylibrary.el ends here