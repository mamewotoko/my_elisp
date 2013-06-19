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
;; tex�Υ��顼����ɤä����Τ����Ӥޤ���
;; ���
;;   ���顼���Ϥν�
;;   ���顼���Ф��Ȥ��ˤϥ�������Ԥ���Ƭ��?���Ф롣
;;   ���ο��Ծ�˥��顼�ս��ɽ��ʸ�������롣 l.[line_number]
;;   ���顼���Ǥ��ե�����̾�Ϥ��äȾ�ˤ��롣���ɥۥå���.tex��õ����(��)
;;   ** ¿ʬ\input �ˤ��б����ʤ�������Ȥ��Ϻ�Ԥ����ä��Ȥ�

;;; Code:

;;��³��Ū�������Ѥ��ꡣ

(defun my-platex-get-error-line ()
  (search-backward-regexp "^l.\\([0-9]+\\) ")
  (buffer-substring (match-beginning 1)
		    (match-end 1)))

(defun my-platex-get-error-filename ()
  (search-backward-regexp "^(\\([a-zA-Z0-9._-]+.tex\\)$")
  (buffer-substring (match-beginning 1)
		    (match-end 1)))

;; ����������֤� ? �ιԤ�
;;
(defun my-platex-jump-to-error (error-message)
  (and (eq (aref error-message 0) ??)
      (let ((initial-point (point)) ;; initial-point �ؤ� goto-char�����٤�
	    ;; �ƤӽФ���������� (��)
	    (line-number (my-platex-get-error-line))
	    (filename (my-platex-get-error-filename)))
	(goto-char initial-point)
	(my-error-jump-to-point filename line-number))))

(provide 'mytexextend)

;;; mytexextend.el ends here
