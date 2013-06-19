;;; myincrement.el ---    Last modified: Sun Jul 07 23:01:05 2002

;; Copyright (C) 2001 by Free Software Foundation, Inc.

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
;; �꡼������ޤ� my-increment-plus-new-format ����Ͽ����
;; ������Ͽ�����꡼�������˸��������Ϥ��٤ƥ��󥯥���Ȥ���롣
;; delta���ͤ���ꤹ���delta�������󥯥���Ȥ���롣
;; ���󥵡��Ȥ���������Ѥ������ƥ��󥯥���Ȥ��줿���ȤΥե����ޥåȤ���Ͽ�����
;; �����֤��⥵�ݡ���
;; ��Ԥϥ꡼����󥳥ԡ��Υ����Ȥ��ʤ������˥Х���ɤ��Ƥ���Τ�
;; ��Ͽ�ؿ��ˤϥ꡼������ξü�򤷤Ƥ�����������Ϥ����Ȥˤ�����
;; ���路���إ�פ�ɬ�פˤʤä���񤯡�

;;; Code:
(defvar my-increment-number-regexp "[0-9]")
(defvar my-increment-not-number-regexp "[^0-9]")
(defvar my-increment-current-format-function nil)

;(require 'cc-mode)

;;------------------------------------------------------------
;; ��Ͽ���롣
;;
(defun my-increment-plus-new-format (start end)
  (interactive)
  (setq my-increment-current-format-function
	(my-increment-get-numbers start end)))

;;------------------------------------------------------------
;;�ꥹ�Ȥ��Ф���ޥåס�
;;
(defun my-map (f lst)
  (let ((result '()))
    (while lst
      (setq result (cons (apply f (list (car lst))) result))
      (setq lst (cdr lst)))
    (reverse result)))

(defun my-increment-by-current-format-function (&optional delta)
  (interactive)
  (if (null delta)
      (setq delta 1))
  (if my-increment-current-format-function
      (let* ((myformat (cadr my-increment-current-format-function))
	     (myargs
	      (cddr my-increment-current-format-function))
	     (new-args (my-map '(lambda (x) (+ x delta)) myargs))
	     (new-exp (cons 'format (cons myformat new-args))))
	(setq my-increment-current-format-function new-exp)
	(insert-string (eval new-exp)))))
  
;;------------------------------------------------------------
;; ��Ͽ�ؿ�
;;   �����򤹤٤�"%d"�ˤ��ƥե����ޥåȺ�����
;;   ������ꥹ�Ȥˤ��롣
;;   
(defun my-increment-get-numbers (start end)
  (interactive)
  (let ((start (region-beginning))
	(end (region-end))
	(number-list '())
	(flag t)
	(result-format "")
	first-element
	tmp
	current-substring)
    (goto-char start)
    (while (and (search-forward-regexp my-increment-number-regexp end t) flag)
      (setq tmp (- (point) 1)) ;�ޥå��������ο��������Τ�ʤ��������ڡ������⤷��ʤ�
      (setq current-substring (buffer-substring start tmp))
      (setq result-format (concat result-format current-substring "%d"))
      (setq first-element (string-to-number (buffer-substring tmp end)))
      (setq number-list (cons first-element number-list))
      (setq flag (search-forward-regexp my-increment-not-number-regexp end t))
      (setq start (- (point) 1)))
    (if flag ;�Ǹ�Ͽ����Ǥʤ�
	(setq result-format (concat result-format (buffer-substring start end))))
    (setq number-list (reverse number-list))
    (cons 'format (cons result-format number-list))))

(defun my-increment-repeat ()
  (interactive)
  (if  my-increment-current-format-function
      (let ((times (read-number "Times: ")))
	(while (> times 0)
	  ;;delta���ݡ��Ȼ��ˤɤ����뤫?
	  (my-increment-by-current-format-function)
	  (setq times (- times 1))))))

(defun my-increment-tell-current-format ()
  (interactive)
  (let ((current-format (cadr my-increment-current-format-function)))
    (message "%s" current-format)))

;;------------------------------------------------------------
;; ������Υ��ԡ�
;; ���ݤʤΤ��ѹ��ս�ϰ�ս�
;; �����ͽ��ʤĤޤ�ϼ�������Ƥ��ʤ���
;;   Block: # �򥵡�����������ñ�����������
;;   �����դ�������
;; ���� ----------------------------------------
;;   my-increment-lambda-copy-region-register�ǥ꡼������format�����
;; ʸ����Ȥ�����Ͽ���롣��Ͽ�Ǥ���ʸ����ϰ�Ĥ����ˤ��Ƥ��롣(�����
;; �����Ȥ����ʤ��Τ�)��Ͽ�κݤ�%s��Ĥ���Ŭ���񼰻���Ҥ򤤤��(%s�Τ�
;; ���ݡ���)
;;   my-increment-lambda-copy-yank ����Ͽ�����꡼�����ν񼰻���Ҥ�
;; ���ʤ����Ǥ�����롣

(defvar lambda-copy-region-current-format nil)

(defun my-count-format (string)
  (let ((len (length string))
	(result 0)
	(counter 0))
    (while (< counter len)
      (if (eq (aref string counter) ?%)
	  (setq result (+ result 1)))
      (setq counter (+ counter 1)))
    result))

(defun my-increment-lambda-copy-region-register-function (start end)
  (resize-minibuffer-mode)
  (let* ((obj (buffer-substring start end))
	 (format-sentence (read-string "format: " obj))
	 (number-of-format (my-count-format format-sentence)))
    (setq lambda-copy-region-current-format (cons format-sentence number-of-format))
  (resize-minibuffer-mode -1)))

(defun my-increment-lambda-copy-region-register ()
  (interactive)
  (if (region-active-p)
      (my-increment-lambda-copy-region-register-function (region-beginning)
							 (region-end))))

(defun my-increment-lambda-copy-yank ()
  (interactive)
  (if lambda-copy-region-current-format
      (let* ((element (read-string "element: "))
	     (current-format (car lambda-copy-region-current-format))
	     (number-of-format (cdr lambda-copy-region-current-format))
	     (element-list (make-list number-of-format
				      element)))
	(insert-string (eval (cons 'format (cons current-format element-list)))))))

(provide 'myincrement)
;;; myincrement.el ends here
