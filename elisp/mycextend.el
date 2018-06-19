;; mycextend.el --- 
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/ocaml #

;;; Commentary:
;; 他のコードとの依存関係を調べていないが、 mylibrary.elは必要。mycompile.elも
;; 必要。(class名推測)
;;
;; デバッグの容易さとコード領域の削減のためコードをよりシンプルにした。
;;
;; for文の挿入はリージョン指定時もできるようにサポート
;; 
;; 注意:
;;   defaultでクラス名を指定する特殊ヘッダのクラス名の終りの文字
;; (class-name-declaration-end)がmycompile-endと同じ#になっています。
;; これは、mycompile.elについても言えることなのですが、C/C++だと、
;; マクロの先頭に#をつけるために#をつけ忘れた名前指定はマクロの先頭
;; の#にマッチして断りもなく変なクラス名を挿入することがあります。
;; 対策として、limitを十分小さくするか、(adhoc)mycompile-end
;; (class-name-declaration-end)の文字列を#以外の文字列に変更して
;; (本質的)使用してください。

;; エラージャンプはmycompile.elに入れた方が良かったかも。このエラージャンプはgmakeから呼び
;;出したgcc(g++)だとうまく行かないかも知れませんん。(makefile内でcdして他のディレクトリで
;;作業した場合。もちろん作業ディレクトリ内のコンパイルに対するエラーは大丈夫)

;;; Code:
;;変数参照するので必要
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
  "  特殊ヘッダ情報、履歴、ソースの文面ファイル名からクラス名を推測しメソッドのヘッダ
のみを挿入する。断りなしに挿入するのは履歴まで。それ以下はミニバッファからクラス名を
読みとる。それ将来的にはヘッダファイルのプロトタイプ宣言からメソッドを挿入したり、そ
の逆も実装するかもしれない。"
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
;; カウンタ使用のfor文挿入
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
;;インデントは正常でないかも知れない。
;; 括弧は違うものに飛ぶかも知れない。
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

;;gccのエラーコードを読む。エラー個所へ飛ぶ
(defvar gcc-error-top "In function")

;;------------------------------------------------------------
;; バッファ名はどうでもいいのですがシェルであることが大前提です。
;;
;;
(defvar default-shell-buffer-name "*shell*")

;;------------------------------------------------------------
;;shell出力のみ対応
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
;; シェルが立ち上がってることが前提。
;;
(defun my-c-jump-to-next-error ()
  (interactive)
  (switch-to-buffer-other-window default-shell-buffer-name)
  (next-line 1)
  (my-c-jump-to-error-at-point))

;;------------------------------------------------------------
;;シェルにカーソルがある。その行のエラーメッセージを読む
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
