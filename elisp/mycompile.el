;;; mycompile.el ---    Last modified: Fri Jan 30 23:41:43 2004

;; Copyright (C) 2001 by Free Software Foundation, Inc.
;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <tak@is.s.u-tokyo.ac.jp>
;; Keywords: 

;;; Commentary:
;; ���:
;;  mycompile-execute��ɸ�����Ϥ������Ϥ�����դ���ץ�����¹Ԥ����
;;�ߤޤ�ޤ���������ä���ˤ�shell�Υ��ޥ�ɥ饤��������Τ��褤��
;;�פ��ΤǤ������ޤ�̤���Ǥ���(���ݤʤΤ�)���Ϥ����Ф�ץ����ϼ¹�
;;�Ǥ��ޤ���

;;   default�ǥ��ޥ��̾����ꤹ���ü�إå��Υ��饹̾�ν����ʸ��(mycompile-end)
;; ��#�ˤʤäƤ��ޤ���C/C++���ȡ��ޥ������Ƭ��#��Ĥ��뤿��ˡ�#��Ĥ�˺�줿̾����
;; �꤬����ȥޥ������Ƭ��#�˥ޥå������Ǥ��ʤ��Ѥʥ��饹̾���������뤳�Ȥ�����ޤ���
;; �к��Ȥ��ơ�limit��ʬ���������뤫��(adhoc) mycompile-end��ʸ�����#�ʳ���ʸ����
;; ���ѹ�����(�ܼ�Ū)���Ѥ��Ƥ���������

;; Emacs��Υ�����˥��ޥ�ɤ�����ƥ���ѥ��뤹��С���������ޤ���������ˤ�
;; ���������äơ�shell��gdb��Ω���夲�Ƥ���Ȥ��˥���ѥ��뤹��Ⱥ���ޤ����ޤ���
;; forground�ǥץ����¹Ԥ��Ƥ���Ȥ��Ϥޤ��ޤ�����ޤ�������ѥ����ѤΥ������
;; Ω���夲�Ƥ��ɤ������Ǥ������Хåե���¿���ʤ�ΤϤ���ä�....��


;;; Code: 
(defvar mycompile-command-line-limit 7
  "����ѥ��륳�ޥ�ɤ��ե�������Ƭ���鲿���ܤޤǤ��ϰϤˤ��뤫?")

(defvar mycompile-start "Compile:"
  "����ѥ��륳�ޥ�ɤ���Ƭ�ˤĤ���ʸ����")
(defvar myexecute-start "Execute:"
  "�¹ԥ��ޥ�ɤ���Ƭ�ˤĤ���ʸ����")

(defvar mycompile-end "#"
  "����ѥ��륳�ޥ�ɤκǸ�ˤĤ���ʸ����")
(defvar mycompile-end mycompile-end
  "�¹ԥ��ޥ�ɤκǸ�ˤĤ���ʸ����")

(defvar mycompile-buffer-name "*My Compile*")

(defvar my-change-directory-command-format "pushd %s")

;; �ե�����̾���֤�������ʸ����
(defvar my-this-file-mark "{}")

;;shell�����椬���뤳�ȡ��������뤬Ŭ���ʰ��֤ˤ��뤳�Ȥ��ꤹ�롣
(defun my-input-command-to-shell (command)
  (shell)
  (end-of-buffer)
  (insert-string command)
  (comint-send-input)
  (comint-next-prompt 1))

(defun mycompile-compile ()
  (interactive)
  (let ((current-directory (expand-file-name "."))
	(mycompile-command (mycompile-get-command mycompile-start
						mycompile-end
						mycompile-command-line-limit)))
    (if (> (count-windows) 1)
	(other-window-except-minibuffer-window) ;;minibuffer�������....
      (split-window-vertically))
    (my-input-command-to-shell
     (format my-change-directory-command-format current-directory))
    (my-input-command-to-shell mycompile-command)
    (comint-previous-prompt 1)
    (message (concat "Compiled! " mycompile-command))))

;  (display-buffer mycompile-buffer-name))

(defun mycompile-execute ()
  (interactive)
  (let ((current-directory (expand-file-name "."))
	(execute-command (mycompile-get-command myexecute-start
						mycompile-end
						mycompile-command-line-limit)))
    (if execute-command
	(progn 
	  (if (> (count-windows) 1)
	      (other-window-except-minibuffer-window) ;;minibuffer�������....
	    (split-window-vertically))
	  (my-input-command-to-shell
	   (format my-change-directory-command-format current-directory))
	  (my-input-command-to-shell execute-command)
	  (message "Executed!")))))

(defvar my-shell-buffer-name "*shell*")

;; save excursion �Ȥ�ͭ�����ѤǤ��뤫�ʤ�
(defun mycompile-get-command (start-string end-string limit)
  (let ((init-position (point))
	(result ""))
    (goto-line (+ limit))
    (let ((search-end (- (point) 1)))
      (beginning-of-buffer)
      (if (re-search-forward start-string search-end t)
	  (let ((command-beginning (point)))
	    (if (re-search-forward end-string search-end t)
		(progn (backward-char (length end-string))
		       (let ((command-end (point)))
			 (setq result (buffer-substring command-beginning
							command-end))))
	      (message "There is no end mark"))
	    (message "There is no start mark"))))
    (goto-char init-position)
    result))

(defun my-pushd-current-directory ()
  (interactive)
  (let ((target-dir (expand-file-name ".")))
    (if (> (count-windows) 1)
	(other-window-except-minibuffer-window) ;;minibuffer�������....
      (split-window-vertically))
    (my-input-command-to-shell (concat "pushd " target-dir))))

(provide 'mycompile)
;;; mycompile.el ends here
