;;; myhtmlmode.el --- 
;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>

;;; Commentary:

;; やたらリージョン指定します。
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 更新履歴
; 2002/ 5/23
;  my-html-browse-locationを追加。
;  あるファイルに関連しているWebページをみる
;  結果の確認とか
; 2003/ 8/ 3
;  s/BORDER/border/;

;;; Code:

(defvar my-html-tag-start-len-regexp
  "\\(<[^<>]+>\\)+")

(defvar my-html-tag-end-len-regexp
  "\\(</[^<>]+>\\)+")

(defvar my-html-tag-alist
  '(("font". "color=\"#\"")
    ("a"   . "href=\"\"")
    ("div" . "align=\"\"")
    ("img" . "src=\"\" alt=\"\"")))

(defvar my-html-location-header "Location:")
(defvar my-html-location-end "#")
(defvar my-html-location-header-line-limit 15)

;(defconst my-html-get-title-program (expand-file-name "~/bin/get_html_title.pl"))
;(defun my-html-create-reference (url)
;  (let* ((url (replace-in-string url "'" ""))
;	 (command
;	  (concat my-html-get-title-program " '" url "'"))
;	 (title (shell-command-to-string command)))
;    (if (equal title "")
;	nil
;      (format "<a href=\"%s\">%s</a>" url title))))

(defun my-html-tag-insert (tag &optional close)
  (interactive "sTag: ")
  (let ((here (point))
	(back nil)
	(addition (my-search-alist2 tag my-html-tag-alist 'equal)))
    (if (tooltip-region-active-p)
	;;リージョンをはさむ
	(progn 
	  (setq back t)
	  (let* ((start (concat "<" tag))
		 (end (concat "</" tag ">"))
		 (r-beginning (region-beginning))
		 (r-end (region-end))
		 (tmp 0))
	    (if (> r-beginning r-end)
		(progn
		  (setq tmp r-end)
		  (setq r-end r-beginning)
		  (setq r-beginning tmp)))
	    ;;リージョンの最後に閉じタグ
	    (goto-char r-end)
	    (insert end)
	    ;;リージョンの最初に開きタグ
	    (goto-char r-beginning)
	    (insert start)
	    (if addition
		(progn 
		  (setq back nil)
		  (insert (concat " " addition ">")) ;ad hoc
		  (backward-char 2))
	      (insert ">"))))
      (if close
	  (progn
	    (insert (format "<%s></%s>" tag tag))
	    (search-backward "><")
	    (if addition
		(progn 
		  (setq back nil)
		  (insert (concat " " addition))
		  (backward-char 1))
	      (forward-char 1)))
	(insert (concat "<"
			       (concat tag (if addition (concat " " addition) "")
				       ">"))))
    (if back (goto-char here)))))


;;------------------------------------------------------------
;; スタート位置にカーソルをおく
;;
(defun my-html-copy-format-yank ()
  (interactive)
  (if (tooltip-region-active-p)
      (let* ((start (region-beginning))
	    (end (region-end))
	    (element (buffer-substring start end)))
	(delete-region start end)
	(insert (format lambda-copy-region-current-format 
			       element)))))
  
(defun my-html-copy-format-register-command ()
  (interactive)
  (if (tooltip-region-active-p)
      (my-html-copy-format-register-function (region-beginning) (region-end))))

(defun my-html-copy-format-register-function (start end)
  (let ((here (point)))
    (goto-char start)
    (if (re-search-forward my-html-tag-start-len-regexp)
	(let (start-tags)
	  (setq start-tags (buffer-substring start (point)))
	  (if (search-forward "<")
	      (let ((end-tags (buffer-substring (- (point) 1) end)))
		(setq lambda-copy-region-current-format
		      (concat start-tags
			      "%s"
			      end-tags))))))))

(defun my-html-insert-space ()
  (interactive)
  (insert "&nbsp;"))

(defun my-html-insert-br ()
  (interactive)
  (insert "<br />"))

(defun my-html-insert-paragraph
  (interactive)
  (insert "<p>"))

(defun my-html-insert-simple-table ()
  (interactive)
  (insert "<table border=\"0\">\n<tr><td></td><td></td></tr>\n</table>\n")
  (previous-line 2)
  (search-forward "td")
  (forward-char 1))

(defvar my-html-target-regexp "\\(name\\|NAME\\)=\"")
;;------------------------------------------------------------
;; 一番近いターゲット名でbrowseする
;; ちなみにmeta nameでもマッチします。気にしない。
(defun my-html-browse-nearest-target ()
  (interactive)
  (let ((here (point))
	(file-name (buffer-file-name)))
    (if (search-backward-regexp my-html-target-regexp 0 t)
	(progn
	  (search-forward "\"")
	  (let ((target-name-start (point)))
	    (search-forward "\"")
	    (let* ((target-name-end (- (point) 1))
		   (target-name (buffer-substring target-name-start
						  target-name-end))
		   (url (concat file-name "#" target-name)))
	      (browse-url url))))
      (message "Can't find any targets!"))
    (goto-char here)))

(defvar href-sharp-limit 20)
;;------------------------------------------------------------
;; targetへジャンプ
;; flag = nil でファイルを開く
(defun my-html-jump ()
  (interactive)
  (let ((here (point))
	(flag t)
	obj name start end)
    (if (search-backward-regexp "\\(href\\|name\\|NAME\\)=\"" 0 t)
	(let ((matched-point (point)))
	  (if (eq (char-after) ?h)
	      ;; hrefからジャンプ
	      (progn
		(if (search-forward "\""
				    (+ matched-point href-sharp-limit)
				    t) ;ここらへんのエラー処理は？
		    (progn
		      (if (eq (char-after) ?#)
			  (forward-char 1)
			(setq flag nil))
		      (progn (setq start (point))
			     (search-forward "\"")
			     (setq end (- (point) 1))
			     (setq name (buffer-substring start end))
			     (if flag
				 (setq obj (concat my-html-target-regexp name))
			       (setq obj name))))))
	    ;;nameからじゃーんぷ
	    (progn 
	      (search-forward "\"") ;ここらへんのエラー処理は？
	      (setq name (thing-at-point 'filename))
	      (setq obj (concat "href=\"#" name))))
	  (if flag
	      (progn
		(beginning-of-buffer)
		(if (not (search-forward-regexp obj (point-max) t))
		    (progn
		      (message "Can't find target %s (regexp %s)" name obj)
		      (goto-char here))))
	    (find-file-other-window obj)))
	    (progn 
	      (message "Can't find anything"))
	(goto-char here))))

(defun my-html-insert-table-column ()
  (let ((here (point)))
    (insert "<tr><td></td><td></td></tr>")
    (goto-char here)
    (forward-char 8)))
  
(defun my-html-insert-item-menu (&optional index)
  (interactive "P")
  (message "B)lock L)i F)ont A)nchor U)nder B)OLD I)talic T)able C)ol H)ttp M)ailto")
  (let ((sw (selected-window))
	(local-index (or index (read-char))))
    (message nil)
    (cond
     ((= local-index ?b) (my-html-tag-insert "blockquote" t))
     ((= local-index ?l) (my-html-tag-insert "li" t))
     ((= local-index ?f) (my-html-tag-insert "font" t))
     ((= local-index ?a) (my-html-tag-insert "a" t))
     ((= local-index ?u) (my-html-tag-insert "u" t))
     ((= local-index ?U) (progn (my-html-tag-insert "ul" t)
				(insert "\n\n")
				(backward-char 1)
				))
     ((= local-index ?B) (my-html-tag-insert "b" t))
     ((= local-index ?i) (my-html-tag-insert "i" t))
     ((= local-index ?I) (my-html-tag-insert (read-input "tag: ") t))
     ((= local-index ?t) (my-html-insert-simple-table))
     ((= local-index ?c) (my-html-insert-table-column))
     ((= local-index ?C) (progn (insert "<!--  -->")
				(backward-char 4)))
     ((= local-index ?h) (insert-string "http://")) ;httpをインサート
     ((= local-index ?m) (insert "mailto:")) ;mail 
     ((= local-index ?L) (insert "&lt;"))  
     ((= local-index ?G) (insert "&gt;")) 
     ((= local-index ?s) (insert "<table border=\"1\">
<tr><td><pre>

</pre></td></tr>
</table>")) ;;ソースを表示。ボーダー1の枠で囲む
     )))

(provide 'myhtmlmode)

;;; myhtmlmode.el ends here
