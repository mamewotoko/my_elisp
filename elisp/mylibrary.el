;;; mylibrary.el --- for me
;;;                               Last modified: Wed Jun 19 20:58:25 2013
;; FTP Directory: sources/emacs #
;; Author: MASUYAMA Takashi <mamewo@dk9.so-net.ne.jp>
;; Keywords: 

(defvar my-c-include-path-list
  (list "/usr/include/g++-3"
	"/usr/include/g++-3/std"
	"/usr/include"
	"/usr/include/sys/"
	"/usr/local/include"))

(defvar my-lisp-path
  (expand-file-name "~/lib/emacs/lisp"))

(defvar my-path
  (append (cons my-lisp-path
		my-c-include-path-list)
	  load-path))

(defun file-name-prefix (filename)
  (let ((dot-position (posix-string-match "\\." filename nil)))
    (substring filename 0 dot-position)))

(defun file-name-suffix (filename)
  (let* ((dot-position (posix-string-match "\\." filename nil)))
    (substring filename (+ dot-position 1) nil)))

;; ͥ����
;;  �꡼����� -> point
(defun my-word-at-position ()
  (if (tooltip-region-active-p)
      (buffer-substring (region-beginning)
			(region-end))
    (thing-at-point 'word)))

(defun manual-entry-at-position ()
  "�꡼����������Ϥ��Υ꡼������ñ��Ȥ��Ƽ����������Ǥʤ����ϡ�
����������֤�ñ���thing-at-point�Ǽ������롣�����ơ������manual-entry
���Ϥ�������"
  (interactive)
  (let ((obj (my-word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
      (progn
	(other-window 1)
	(manual-entry obj)))))

(defun other-window-except-minibuffer-window ()
  (other-window 1)
  (if (string-match " *Minibuf" (buffer-name))
      (other-window 1)))

;; ͥ����
;;  �꡼����� -> point
(defun apropos-at-position ()
  (interactive)
  (let ((obj (my-word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
    (apropos obj))))

;;�ѥ��ˤ��ҥ�Ȥ�Ĥ��뤳�Ȥ�ͤ��Ƥ��ɤ������ݡ�
(defun my-find-library-path (filename path-list)
  (let ((path (locate-library filename t path-list)))
    (if (null path)
	;; try
	(progn (setq path (locate-library filename nil path-list))
	       (if (null path)
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

;;;;;;;;;List��� ���ôؿ�
;;Ϣ������Υ�����
(defun my-search-alist (key alist)
  (while (and (not (null alist))
	      (not (eq key (caar alist))))
    (setq alist (cdr alist)))
  (if (null alist)
      nil
    (cadar alist)))

(defun my-search-alist2 (key alist equality)
  "�������Ѥ���Ϣ�ۥꥹ�Ȥ򥵡������ҥåȤ�����Ϣ���ͤ��֤�"
  (while (and (not (null alist))
	      (not (eval (list equality key (caar alist)))))
    (setq alist (cdr alist)))
  (if (null alist)
      nil
    (cdar alist)))

(defun add-to-alist (key value alist)
  (let ((new-element (list 'cons key value)))
    (eval (list 'setq alist (list 'cons new-element alist)))))

;(defvar user-url "http://www10.u-page.so-net.ne.jp/dk9/mamewo/")
(defvar user-url "http://www002.upp.so-net.ne.jp/mamewo/")
(defvar signature-file (expand-file-name "~/.signature"))

;;------------------------------------------------------------
;; ʸ�����Ƭ�Υ��ڡ������֤��������
;; ����ʤ��®���������Ϥ����񸻤⾯�ʤ��Ϥ���
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

(defun insert-date-yy/mm/dd-format ()
  (interactive)
  (let ((time-string (time-stamp-yyyy/mm/dd)))
    (if (eq (aref time-string 5) ?0)
	(aset time-string 5 ?  ))
    (if (eq (aref time-string 8) ?0)
	(aset time-string 8 ?  ))
    (insert-string time-string)))

(defun insert-date-yy_mm_dd-format ()
  (interactive)
  (let ((time-string (time-stamp-yyyy/mm/dd)))
    (aset time-string 4 ?_)
    (aset time-string 7 ?_)
    (insert-string time-string)))

;;--------------------------------------------------
;; �Ŀ;���ʤɥ����Х�ʥ����ƥ�
;;
(defconst univ-url "http://www.yl.is.s.u-tokyo.ac.jp/~tak/")
(defun my-insert-item-menu (&optional index)
  (interactive "P")
  (message "N)ame D)ate U)rl M)ail S)ign F)ilename P)refix T)ime-mark C)ompile")
  (let ((local-index (or index (read-char))))
    (message nil)
    (cond
     ((= local-index ?a) (insert-string "Takashi Masuyama <mamewo@dk9.so-net.ne.jp>"))
     ((= local-index ?n) (insert-string (user-full-name)))
     ((= local-index ?d) (insert-date-yy/mm/dd-format))
     ((= local-index ?f) (insert-date-yy_mm_dd-format))
     ((= local-index ?u) (insert-string user-url))
     ((= local-index ?u) (insert-string univ-url))
     ((= local-index ?m) (insert-string (user-mail-address)))
     ((= local-index ?M) (insert-string "mamewo@okuiaki.com"))
     ((= local-index ?s) (insert-file signature-file))
     ((= local-index ?f) (insert-file-name))
     ((= local-index ?p) (insert-file-name-prefix))
     ((= local-index ?t) (insert-string time-stamp-start))
     ((= local-index ?l) (insert-string my-line))
     ;;; mycompile.el
;     ((= local-index ?c) (
     ((= local-index ?c)
      (progn 
	(insert-string (format "%s %s" mycompile-start mycompile-end))
	(backward-char (+ (length mycompile-end) 1))))
     (t (message "No such command!"))
)))

(defun my-search-forward-my-word-at-position ()
  (interactive)
  (search-forward (my-word-at-position)))

(defun my-search-backward-my-word-at-position ()
  (interactive)
  (search-backward (my-word-at-position)))

;;------------------------------------------------------------
;;�ե�ѥ���guess���������ʤ�
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
	  (my-find-library-path (concat filename ".el") expanded-path)))))

(defun my-measure-length-of-region ()
  (interactive)
  (if (tooltip-region-active-p)
      (message "length: %d" (abs (- (region-end) (region-beginning))))))

;(defun my-copy-line ()
;  (interactive)
;  (beginning-of-line)
;  (let ((start (point)))
;    (end-of-line)
;    (copy-region-as-kill start (point)))
;  (message "copied"))
;(defun my-activate-line ()
;  (interactive)
;  (beginning-of-line)
;  (push-mark)
;  (end-of-line)
;  (activate-region))

(defvar my-insert-file-info-not-hold-between-format
  "%s %s\t%s\n%s %s %s %s\n%s%s\n")

(defun my-insert-file-info (&optional comment-start compile-command original-format)
  (interactive)
  (if (not original-format)
      (setq original-format my-insert-file-info-not-hold-between-format))
  (if (not compile-command)
      (setq compile-command ""))
  (insert-string
   (format my-insert-file-info-not-hold-between-format
	   comment-start
	   (file-name-nondirectory (buffer-file-name))
	   time-stamp-start
	   comment-start
	   mycompile-start
	   compile-command
	   mycompile-end
	   comment-start
	   my-line)))

;;------------------------------------------------------------
;;start end�ϻ��ꤹ��ʤ���٤�
;;ü�ϥ����Ȥ���Ƥ��ʤ����ɤ�
(defun my-regional-insert (start-string end-string &optional start end)
"start end�ϻ��ꤹ��ʤ���٤�
���֤����ʤ���"
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
  
(defun my-get-image-size (filename &optional scale)
  (let ((data (shell-command-to-string
	       (concat "file " filename))))
    (string-match "\\([0-9]+\\) x \\([0-9]+\\)" data)
    (let* ((width-string (match-string 1 data))
	   (height-string (match-string 2 data))
	   (s (or scale 1.0))
	   (width (* scale (string-to-int width-string)))
	   (height (* scale (string-to-int height-string))))
      (format "width=%d height=%d" width height))))

;; �ǥ��쥯�ȥ����ؿ����ۤ���
;(defvar make-my-page-table ()

;(defun my-make-homepage-table (lst)
;  (if lst
;      (let ((filename (car lst))
;	    (remain (my-make-homepage-table (cdr lst))))
;	(if (string-match "\\.html$" filename)
;	    (cons (cons filename filename) remain)
;	  remain))
;    nil))

;; ��ϥե�����������㤷���ؿ��ƽФΥͥ��Ȥ������뤬�ͥ��Ȥ�
;; �����Ƽ¹ԤǤ��ʤ���礬����Τ� while �ˤ�����
(defun my-make-homepage-table-by-while (lst)
  (let ((result nil))
    (while lst
      (let ((filename (car lst)))
	    (setq lst (cdr lst))
	    (if (string-match "\\.html$" filename)
		(setq result (cons (cons filename filename) result)))))
    (reverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; �Ŀ�Ū�˥ۡ���ڡ����� /home/tak/homepage/ ���Խ����ƥ��åץ��ɤ��Ƥ���
;; ��ʬ�Υڡ����򸫤�Τˡ��ޤ���ʬ�Υڡ����ˤɤ�ʥե����뤬���뤫���Τ��ܰ¤�
;; ���ơ� /home/tak/homepage/ �ˤ���ե������Ĥ����� /home/tak/homepage/ ��
;; ����ե��������� .html ��ĥ�Ҥ��Ĥ�����Τ�Ȥꤢ���ơ�����򥿥��������
;; �Ǥ���褦�ˤ��롣
(defvar my-page-base "http://www002.upp.so-net.ne.jp/mamewo/")
(defvar my-page-local-base "/home/tak/homepage/")

(defun my-browse-my-page (word)
  (browse-url (concat my-page-base word)))
(defun my-browse-my-page-interface ()
  (interactive)
  (my-browse-my-page 
   (completing-read "Browse: "
		    (my-make-homepage-table-by-while
		     (directory-files my-page-local-base)))))

;; left  (f (f (f s a1) a2) a3)
;; right (f a1 (f a2 (f a3 s)))
(defun my-fold-left (f s lst)
  (if (null lst)
      s
    (funcall f (car lst) (my-fold-left f s (cdr lst)))))

(defun my-join (lst)
  (my-fold '(lambda (x y) (concat x " " y)) "" lst))

(provide 'mylibrary)
;;; mylibrary.el ends here