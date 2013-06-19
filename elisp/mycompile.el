;;; mycompile.el ---    Last modified: Fri Jan 30 23:41:43 2004

;; Copyright (C) 2001 by Free Software Foundation, Inc.
;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <tak@is.s.u-tokyo.ac.jp>
;; Keywords: 

;;; Commentary:
;; 注意:
;;  mycompile-executeは標準入力から入力を受け付けるプログラムを実行すると
;;止まります。これを解消するにはshellのコマンドラインに入れるのがよいと
;;思うのですが、まだ未解決です。(面倒なので)出力だけ出るプログラムは実行
;;できます。

;;   defaultでコマンド名を指定する特殊ヘッダのクラス名の終りの文字(mycompile-end)
;; が#になっています。C/C++だと、マクロの先頭に#をつけるために、#をつけ忘れた名前指
;; 定があるとマクロの先頭の#にマッチして断りもなく変なクラス名を挿入することがあります。
;; 対策として、limitを十分小さくするか、(adhoc) mycompile-endの文字列を#以外の文字列
;; に変更して(本質的)使用してください。

;; Emacs上のシェルにコマンドを入れてコンパイルするバージョンを作りました。これにも
;; 難点があって、shellでgdbを立ち上げているときにコンパイルすると困ります。また、
;; forgroundでプロセス実行しているときはまたまた困ります。コンパイル用のシェルを
;; 立ち上げても良さそうですが、バッファが多くなるのはちょっと....。


;;; Code: 
(defvar mycompile-command-line-limit 7
  "コンパイルコマンドがファイル先頭から何行目までの範囲にあるか?")

(defvar mycompile-start "Compile:"
  "コンパイルコマンドの先頭につける文字列")
(defvar myexecute-start "Execute:"
  "実行コマンドの先頭につける文字列")

(defvar mycompile-end "#"
  "コンパイルコマンドの最後につける文字列")
(defvar mycompile-end mycompile-end
  "実行コマンドの最後につける文字列")

(defvar mycompile-buffer-name "*My Compile*")

(defvar my-change-directory-command-format "pushd %s")

;; ファイル名と置き換える文字列。
(defvar my-this-file-mark "{}")

;;shellに制御があること。カーソルが適当な位置にあることを仮定する。
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
	(other-window-except-minibuffer-window) ;;minibufferに入ると....
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
	      (other-window-except-minibuffer-window) ;;minibufferに入ると....
	    (split-window-vertically))
	  (my-input-command-to-shell
	   (format my-change-directory-command-format current-directory))
	  (my-input-command-to-shell execute-command)
	  (message "Executed!")))))

(defvar my-shell-buffer-name "*shell*")

;; save excursion とか有効利用できるかなぁ
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
	(other-window-except-minibuffer-window) ;;minibufferに入ると....
      (split-window-vertically))
    (my-input-command-to-shell (concat "pushd " target-dir))))

(provide 'mycompile)
;;; mycompile.el ends here
