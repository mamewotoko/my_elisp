;; mycextend.el --- 
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/ocaml #

;;; Commentary:
;; ¾�Υ����ɤȤΰ�¸�ط���Ĵ�٤Ƥ��ʤ����� mylibrary.el��ɬ�ס�mycompile.el��
;; ɬ�ס�(class̾��¬)
;;
;; �ǥХå����ưפ��ȥ������ΰ�κ︺�Τ��ᥳ���ɤ��ꥷ��ץ�ˤ�����
;;
;; forʸ�������ϥ꡼�����������Ǥ���褦�˥��ݡ���
;; 
;; ���:
;;   default�ǥ��饹̾����ꤹ���ü�إå��Υ��饹̾�ν����ʸ��
;; (class-name-declaration-end)��mycompile-end��Ʊ��#�ˤʤäƤ��ޤ���
;; ����ϡ�mycompile.el�ˤĤ��Ƥ�����뤳�ȤʤΤǤ�����C/C++���ȡ�
;; �ޥ������Ƭ��#��Ĥ��뤿���#��Ĥ�˺�줿̾������ϥޥ������Ƭ
;; ��#�˥ޥå������Ǥ��ʤ��Ѥʥ��饹̾���������뤳�Ȥ�����ޤ���
;; �к��Ȥ��ơ�limit��ʬ���������뤫��(adhoc)mycompile-end
;; (class-name-declaration-end)��ʸ�����#�ʳ���ʸ������ѹ�����
;; (�ܼ�Ū)���Ѥ��Ƥ���������

;; ���顼�����פ�mycompile.el�����줿�����ɤ��ä����⡣���Υ��顼�����פ�gmake����Ƥ�
;;�Ф���gcc(g++)���Ȥ��ޤ��Ԥ��ʤ������Τ�ޤ����(makefile���cd����¾�Υǥ��쥯�ȥ��
;;��Ȥ�����硣�������ȥǥ��쥯�ȥ���Υ���ѥ�����Ф��륨�顼�������)

;;; Code:
;;�ѿ����Ȥ���Τ�ɬ��
;(load "mycompile.el")

(defun my-print-variable (f)
  (interactive)
  (let ((variable (read-string "variable: ")))
    (insert-string (format f variable variable))))
	
(defvar my-c-print-format "printf(\"%s: %%s\\n\",%s);\n")
(defvar my-c++-print-format "cout << \"%s: \" << %s << endl;\n")

(defun my-c++-extend-menu (&optional index)
  (interactive "P")
  (message "C)lass F)or H)eader T)ype O)ption M)ethod I)f L)INE I)FDEF p)rintf")
  (let ((sw (selected-window))
	(local-index (or index (read-char))))
    (message nil)
    (cond
     ((= local-index ?c) (my-c-extend-insert-class-declaration))
     ((= local-index ?f) (c++-insert-incremental-for))
     ((= local-index ?i) (my-c-insert-if))
     ((= local-index ?I) (my-c-insert-conditional-compile))
     ((= local-index ?L) (insert-string "__LINE__"))
     ((= local-index ?F) (insert-string
			  "FILE* [input/output] = fopen([filename],\"rw\");"))
     ((= local-index ?m) (my-c++-insert-method-head))
     ((= local-index ?o) (my-c-insert-treat-option))
     ((= local-index ?p) (progn (insert-string "printf(\"\\n\");\n")
				(backward-char 6)))
     ((= local-index ?v) (my-print-variable my-c++-print-format))
     ))) ;insert-optition

(defun my-c-insert-treat-option ()
  (let ((subscript (read-string "index: " "i"))
	(begining-point (point)))
    (insert-string
     (format
      "for(int %s = 1; %s < argc; %s++) {\nif(strcmp(argv[%s],\"\") == 0){\n}\n}\n"
      subscript subscript subscript subscript))
    (c-indent-region begining-point (point))
    (previous-line 3)
    (search-forward "\"")))

(defun my-c++-insert-method-head ()
  "  �ü�إå��������򡢥�������ʸ�̥ե�����̾���饯�饹̾���¬���᥽�åɤΥإå�
�Τߤ��������롣�Ǥ�ʤ�����������Τ�����ޤǡ�����ʲ��ϥߥ˥Хåե����饯�饹̾��
�ɤߤȤ롣���쾭��Ū�ˤϥإå��ե�����Υץ�ȥ������������᥽�åɤ����������ꡢ��
�εդ�������뤫�⤷��ʤ���"
  (interactive)
  (let* ((class-name (my-c++-get-classname))
	 (method_header (format "%s::(){\n}\n" class-name)))
    (insert-string method_header)
    (backward-char 6)))

(defun my-make-up-up-name (string)
  (aset string 0 (upcase (aref string 0)))
  (let ((count 0)
	(len (length string))
	(flag nil))
    (while (< count len)
      (let ((here (aref string count)))
	(if flag
	    (aset string count (upcase here)))
	(if (eq here ?_)
	    (setq flag t)
	  (setq flag nil)))
      (setq count (+ count 1)))))

;;------------------------------------------------------------
;; �����󥿻��Ѥ�forʸ����
;;
;;
(defvar c++-insert-incremental-for-start-format
  "for(int %s = 0; %s < ; %s++) {\n")
(defvar c-insert-incremental-for-start-format
  "for(%s = 0; %s < ; %s++) {\n")

(defvar c++-insert-incremental-for-end-string
  "}\n")

(defun c-insert-incremental-for-function (here-format)
  (let* ((variable (read-string "index: "))
	 (start-string
	  (format here-format
		  variable
		  variable
		  variable))
	 (insert-length (+ (length start-string)
			   (length c++-insert-incremental-for-end-string)))
	 start end)
    (if (tooltip-region-active-p)
	(progn
	  (setq start (region-beginning))
	  (setq end (region-end))
	  (my-regional-insert start-string
			      c++-insert-incremental-for-end-string
			      start
			      end)
	  (setq end (+ end insert-length))
	  (goto-char start))
      (progn
	(setq start (point))
	(setq end (+ start insert-length))
	(insert-string
	 (concat start-string c++-insert-incremental-for-end-string))
	(previous-line 2)))
    (c-indent-region start end)
    (search-forward "<")
    (forward-char 1)
    ))

(defun c++-insert-incremental-for ()
  (interactive)
  (c-insert-incremental-for-function c++-insert-incremental-for-start-format))

(defun c-insert-incremental-for ()
  (interactive)
  (c-insert-incremental-for-function c-insert-incremental-for-start-format))

(defun display-start-end ()
  (interactive)
  (message "%d %d" (region-beginning) (region-end)))

(defun my-c-insert-conditional-compile ()
  (interactive)
  (if (tooltip-region-active-p)
      (let ((start (region-beginning))
	    (end (region-end)))
	(my-regional-insert "#ifdef \n" "#endif\n")
	(goto-char start)
	(end-of-line))
    (progn 
      (if (not (eq (char-before)?\n))
	  (insert-char ?\n))
      (insert-string "#ifdef \n#endif\n");
      (previous-line 2)
      (end-of-line))))

;;------------------------------------------------------------
;;����ǥ�Ȥ�����Ǥʤ������Τ�ʤ���
;; ��̤ϰ㤦��Τ����֤����Τ�ʤ���
(defun my-c-insert-if ()
  (interactive)
  (let (start end)
    (if (tooltip-region-active-p)
	(progn
	  (setq start (region-beginning))
	  (setq end (+ (region-end) 7))
	  (my-regional-insert "if() {\n" "}")
	  (goto-char start))
      (progn 
	(setq start (point))
	(insert-string "if() {\n}")
	(setq end (point))))
    (c-indent-region start end)
    (goto-char start)
    (beginning-of-line)
    (search-forward "(")
    (message "%d %d" start end)
    ))

(defun my-c++-open-header-and-source ()
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (suffix (file-name-suffix file-name))
	 (prefix (file-name-prefix file-name))
	 (opposite-suffix
	  (if (equal suffix "h") "cc" "h"))
	 (obj (concat prefix "." opposite-suffix)))
    (find-file-other-window obj)))

(defun my-c-open-header-and-source ()
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (suffix (file-name-suffix file-name))
	 (prefix (file-name-prefix file-name))
	 (opposite-suffix
	  (if (equal suffix "h") "c" "h"))
	 (obj (concat prefix "." opposite-suffix)))
    (find-file-other-window obj)))

;;gcc�Υ��顼�����ɤ��ɤࡣ���顼�Ľ������
(defvar gcc-error-top "In function")

;;------------------------------------------------------------
;; �Хåե�̾�Ϥɤ��Ǥ⤤���ΤǤ���������Ǥ��뤳�Ȥ�������Ǥ���
;;
;;
(defvar default-shell-buffer-name "*shell*")

;;------------------------------------------------------------
;;shell���ϤΤ��б�
;;
; (defun my-ez-length ()
;   (let ((start (point)))
;     (end-of-line)
;     (+ 1 (- (point) start))))

; (defun my-c-jump-to-error-point ()
;   (interactive)
;   (switch-to-buffer-other-window default-buffer-name)
;   (end-of-buffer)
;   (search-backward gcc-error-top)
;   (comint-previous-prompt 1)
;   (if (search-forward gcc-error-top)
;       (progn (next-line 1)
; 	     (my-c-jump-to-error-at-point))))

;;------------------------------------------------------------
;; �����뤬Ω���夬�äƤ뤳�Ȥ�����
;;
(defun my-c-jump-to-next-error ()
  (interactive)
  (switch-to-buffer-other-window default-shell-buffer-name)
  (next-line 1)
  (my-c-jump-to-error-at-point))

;;------------------------------------------------------------
;;������˥������뤬���롣���ιԤΥ��顼��å��������ɤ�
;;
(defun my-c-jump-to-error-at-point ()
  (interactive)
  (beginning-of-line)
  (let ((start (point)))
    (end-of-line)
    (let ((info (buffer-substring start (point))))
      (my-c-jump-to-error-point-sub info t))))

(load "myerrorjump.el")

(defconst gdb-jump-regexp "^#.+ at \\([^ :]+\\):\\([0-9]+\\)")
(defun my-gdb-jump-to-point-sub (error-message &optional print-p)
  (if (string-match gdb-jump-regexp error-message)
      (let ((filename (match-string 1 error-message))
	    (line-number (match-string 2 error-message)))
	(my-error-jump-to-point filename line-number))))

(defvar my-c-search-directory-list (list "."))

(defun my-c-jump-to-error-point-sub (error-message &optional print-p)
  (and (string-match "^\\(.*] \\)?\\(.*\\)$" error-message)
       (let ((splitted (split-string (match-string 2 error-message) ":")))
	 (if (>= (length splitted) 3)
	     (let* ((filename (nth 0 splitted))
		    (line-number (nth 1 splitted))
		    (filepath (locate-library filename t my-c-search-directory-list)))
	       (my-error-jump-to-point filepath line-number))))))

(provide 'mycextend)

;;; mycextend.el ends here
