;;; myjavamode.el --- Last modified: Sat Jul 07 21:06:46 2018
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;; Location: http://www002.upp.so-net.ne.jp/mamewo/sources/emacs/myjavamode.el #

(defvar my-java-api-base "/mnt/other/tak/Doc/docs/api")
(defvar my-java-api-allclasses (concat my-java-api-base "allclasses-frame.html"))
(defvar my-java-api-search-output-buffer "*Shell Command Output*")
(defconst java-namazu-dir "/home/tak/.indexes/java")

(defun my-java-search-api (class &optional kill apropos)
  ;(interactive)
  (let ((search-obj (if apropos class (format "/%s.html" class))))
    ;; 注意 shell-command はステータスコードを返すのと、t,nil
    ;;を返すのがあるようです(バージョン依存)ステータスコードを
    ;;返される場合は 返り値が0であるかを見るように変更して下さい。
    ;; xemacs --> t/nil
    ;; emacs  --> status か??
    ;; (= (shell-command (format "grep %s %s search-obj
    ;;                            my-java-api-search-output-buffer))
    ;;     0) と変更する
    (if (shell-command (format "grep %s %s" search-obj my-java-api-allclasses))
	(progn
	  (if (not kill)
	      (if (> (count-windows) 1)
		  (other-window 1) ;;minibufferに入ると....
		(split-window-vertically)))
	  (switch-to-buffer my-java-api-search-output-buffer)
                                        ;(beginning-of-buffer)
          (goto-char (point-min))
	  (search-forward "\"")
	  (let ((start (point)))
	    (search-forward "\"")
	    (let* ((end (- (point) 1))
		   (path (buffer-substring start end)))
	      (if kill (kill-buffer my-java-api-search-output-buffer))
	      (setq path (concat "file:" my-java-api-base path))
	      (browse-url path))))
      (message "Not found"))))


(defvar my-search-directory-comet
  (list "/home/tak/project/comet-1.0/comet/src/comet/mobile/core"
	"/home/tak/project/comet-1.0/comet/src/comet/mobile/mobile"))

;;(defvar my-search-directories my-search-directory-emfg)
  
(defun my-java-open-source-file (class path-list)
  (let* ((filename (concat class ".java"))
	 (path (locate-library filename nil path-list)))
    (if path
	(find-file-other-window path)
      (message (concat "Not found the file: " filename)))))

(defun my-java-search-api-at-point ()
  (interactive)
  (let ((obj (my-word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
      (my-java-search-api obj t))))

(defun my-java-apropos-search-api-at-point ()
  (interactive)
  (let ((obj (my-word-at-position))) ;; regionあった方が良い
    (if (null obj)
	(message "Irregural Position!")
      (my-java-search-api obj nil t))))


(defun my-java-jump-at-exception (error-message &optional path)
  ;;(and (string-match "javac. [^\(]+(\\([^:]+\\):\\([0-9]+\\))" error-message)
  (and (string-match "\\[javac\\] \\([^ :][^:]+\\):\\([0-9]+\\):"error-message)  ;; lint warning
       (let* ((filename (match-string 1 error-message))
	      (searchpath (or path my-java-search-path))
	      (linenumber (match-string 2 error-message))
	      (filepath (locate-file filename searchpath)))
	 (my-error-jump-to-point filename linenumber))))

(provide 'myjavamode)

;;; myjavamode.el ends here
