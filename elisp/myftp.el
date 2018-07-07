;;; myftp.el --- 
;; Last modified: Sat Jul 07 21:09:14 2018
;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; Keywords: 

;;; Code:
(defconst myftp-dir-start "FTP Directory: ")
(defconst myftp-buffer-name " *FTP RESULT*")
(defconst myftp-base-url "http://www002.upp.so-net.ne.jp/mamewo/")
(defconst myftp-sit-for-second 0.5)
(defvar myftp-hput-path (expand-file-name "~/bin/hput"))
(defvar myftp-hget-path (expand-file-name "~/bin/haget"))
;; hagetが知っている。hagetがファイルを置く場所
(defvar myftp-get-local-path (expand-file-name "~/homepage/"))

(defun myftp-put ()
"現在編集中のファイルをWEBサーバーにアップロードする"
  (interactive)
  (shell-command (format "%s %s" myftp-hget-path (buffer-file-name))))

(defun myftp-put-with-dir ()
"現在編集中のファイルをWEBサーバーにアップロードする"
  (interactive)
  (let* ((dir (myftp-get-path))
	 (real-dir (or dir "."))
	 (process (start-process "PUT" myftp-buffer-name
			        ;;; このプログラムにすべてがある
				(expand-file-name "~/bin/hput")
				"-d" real-dir
				(file-name-nondirectory (buffer-file-name)))))
    (message real-dir)
    (sit-for myftp-sit-for-second) ;;これがないとfailしても成功したといってしまう
    (message (if (= (process-exit-status process) 0)
		 "Succeeded"
	       "Failed"))
    (or (my-html-browse-location) (my-browse-ftp-directory) (message "no location!!"))))

(defun myftp-get (&optional filename)
  "WEBサーバーから myftp-hput-path にファイルをダウンロードする"
  (interactive)
  (let ((process (start-process "GET" myftp-buffer-name
				 ;;; このプログラムにすべてがある
				myftp-hget-path
				(or filename (read-string "filename: ")))))
    (sit-for myftp-sit-for-second) ;;これがないとfailしても成功したといってしまう
    (let ((status (= (process-exit-status process) 0)))
      (message (if (= (process-exit-status process) 0)
		   "Succeeded"
		 "Failed"))
      status
      )))

(defun myftp-get-and-find-file ()
  "myftp-get + そのファイルを開く"
  (interactive)
  (let ((filename (read-input "filename: ")))
    (if (myftp-get filename)
	(find-file (concat myftp-get-local-path filename)))))

;; FTP Directory にはプログラムのソースファイルのパス名がはいっている。
;; それを元にしてURLを作成してブラウザで表示する。
;; Filenameの結合を実装しないでアドホックに。
(require 'tl-str)
(defun my-browse-ftp-directory ()
  (let ((path (myftp-get-path)))
    (if path
	(let* ((epath (eliminate-top-spaces (eliminate-last-spaces path)))
	       (len (length epath))
	       (s (if (= (aref epath 0) ?/) 1 0))
		 (e (if (= (aref epath (- len 1)) ?/) (- len 1) len))
		 (normalized-path (substring epath s e))
		 (filename (file-name-nondirectory (buffer-file-name)))
		 (url (concat myftp-base-url normalized-path "/" filename)))
	  (browse-url url)
	  (message (concat "browsing.. " url))
	  t)
      nil)))
  
(provide 'myftp)

;;; myftp.el ends here
