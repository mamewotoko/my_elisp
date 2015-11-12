;;; mykeymap.el
;;                         Last modified: Thu Nov 12 19:21:18 2015

;; FTP Directory: sources/emacs #
;; Author: Takashi Masuyama <tak@is.s.u-tokyo.ac.jp>
;; Keywords: 

(load "mylibrary.el")
(load "myincrement.el")
;;;;;;;;;;;;;;;;;;;;;;;
;;;キーバインドの変更


(global-set-key "\C-ci" 'my-insert-item-menu)
(global-set-key "\C-cy" 'my-increment-by-current-format-function)
;(global-set-key "\C-c\C-y" 'my-increment-by-current-format-function)
;(global-set-key "\C-c\C-d" 'my-increment-tell-current-format)
(global-set-key "\C-cl" 'my-measure-length-of-region)

;;;ジャンプ
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-c\C-j" 'my-goto-error)

;;;置換
(global-set-key "\C-cr" 'query-replace)
(global-set-key "\C-ct" 'insert-date-yy/mm/dd-format)
(global-set-key "\C-cf" 'insert-file-name)
(global-set-key "\C-cp" 'insert-file-name-prefix)

;;;貼付け
;(global-set-key "\C-n" 'next-line)
(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する

;;;正規表現で検索
(global-set-key "\C-c\C-r" 'search-backward-regexp)
(global-set-key "\C-c\C-s" 'search-forward-regexp)
(global-set-key "\C-c\C-c" 'comment-region)

;;;ブックマークをつける
;;(global-set-key "\C-b" 'bookmark-set)
;;;ブックマークジャンプ
;;(global-set-key "\C-j" 'bookmark-jump)
;;;ブックマークを保存
;;(setq bookmark-save-flag 1)
;;;別のウィンドウに移動
;(global-set-key "\C-f"
;  '(lambda ()
;     (interactive)
;     (other-window -1)))
(global-set-key "\C-v" 'other-window)
;;;コピー
(defun my-copy-primary-selection-and-add-to-my-incremental (&optional start end)
  (interactive)
  (let ((here (point)))
    (if (and (not start))
	(if (tooltip-region-active-p)
	    (progn
	      (setq start (region-beginning))
	      (setq end (region-end)))))
    (copy-region-as-kill start end)
    (my-increment-plus-new-format start end)
    (goto-char here)))

(global-set-key "\C-t" 'copy-region-as-kill)

(global-set-key "\C-c\C-t" 'my-increment-lambda-copy-region-register)
(global-set-key "\C-x\C-y" 'my-increment-lambda-copy-yank)

(global-set-key "\M-n" 'find-include-etc-at-position)
(global-set-key "\M-m" "\C-m")

(global-set-key "\C-o" 'dabbrev-expand)
;バッファをkill
(if (not (featurep 'kill-this-buffer))
    (defun kill-this-buffer ()
      (interactive)
      (kill-buffer (current-buffer))))
;(global-set-key "\C-c\C-k" 'copy-line)

(global-set-key "\C-xk" 'kill-this-buffer)
;(global-set-key "\C-xf" 'find-file)
(global-set-key "\C-xC-o" 'find-file-other-window)
(global-set-key [hiragana-katakana] 'toggle-input-method)
(global-set-key [(control tab)] 'delete-rectangle)
(global-set-key "\C-x\C-o" 'find-file-other-window)
(global-set-key [(control button3)] 'popup-buffer-menu)

(global-set-key "\C-x\C-e" 'edict-search-english)
(global-set-key "\C-x\C-j" 'edict-search-kanji)

;; keyの設定
(global-set-key "\C-h" 'backward-delete-char) ;; Ctrl+h  BSキー

;;;ftp
;(defun ftp-to-sonet ()
;  (interactive)
;  (ftp "ftp10.u-page.so-net.ne.jp"))
;(load "mygrep.el")
(global-set-key "\M-f" 'my-recursive-grep)
(global-set-key "\M-g" 'grep)
;;(global-set-key "\M-t" 'my-grep-this-file)
(global-set-key "\M-c" 'my-cgrep)

;;lisp
;(global-set-key "\M-l" 'lisp-interaction-mode)

(global-set-key [f1] 'help-command)           ;; F1   ヘルプ
;; (global-set-key [(control f2)] 
;;   '(lambda () (interactive)
;;      (switch-to-buffer-other-window (current-buffer))))
;;(global-set-key [f2] 'list-buffers)
(global-set-key [f2] 'manual-entry)
(global-set-key [(meta f2)] 'rename-buffer)
;(global-set-key [(control f4)] 'my-html-browse-nearest-target)
(global-set-key [(meta f4)] 
  '(lambda () (interactive)
     (or (my-html-browse-location)
	 (my-browse-ftp-directory)
	 (message "no location!!"))))

;; (global-set-key [f5] 'manual-entry)
;; (global-set-key [(shift f5)] 'manual-entry-at-position)

;(global-set-key [f7] 'mew)
;(global-set-key [(shift f7)] 'compose-mail-at-position)
;(global-set-key [(control f7)] '(lambda ()
;				  (interactive)
;				  (let ((buffer-name "+inbox"))
;				    (if (get-buffer buffer-name)
;					(switch-to-buffer "+inbox")
;				      (message "inbox is not alive.")))))
;(load "mygoogle.el")
;(global-set-key [(meta f7)] 'my-google-search-in-minibuffer)

(global-set-key [f5] (lambda () (interactive) (progn (shell "*f5-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f6] (lambda () (interactive) (progn (shell "*f6-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f7] (lambda () (interactive) (progn (shell "*f7-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f8] (lambda () (interactive) (progn (shell) (set-buffer-process-coding-system 'utf-8 'utf-8))))

(global-set-key [(control f5)] '(lambda () (interactive) (my-pushd-current-directory "*f5-shell*")))
(global-set-key [(control f6)] '(lambda () (interactive) (my-pushd-current-directory "*f6-shell*")))
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f7-shell*")))

(global-set-key [(control f8)] 'my-pushd-current-directory)

(global-set-key "\C-xm" 'ignore)
(global-set-key [f9] 'apropos)
(global-set-key [(control f9)] 'customize-apropos)
(global-set-key [(shift f9)] 'apropos-at-position)
(global-set-key [f10] 'namazu)
(global-set-key [(shift f10)] 
  (lambda () (interactive) 
    (let ((word (my-word-at-position)))
      (namazu 0 namazu-default-dir word))))

(global-set-key [zenkaku-hankaku] 'toggle-input-method)

;(load "mynamazu.el")
;(global-set-key [(control f10)] 'set-namazu-dir-of-buffer-command)
(global-set-key [(meta f10)] 'irchat)
;(global-set-key [(shift f10)] 'bbdb)

(global-set-key [f11]
  '(lambda ()
     (interactive)
     (switch-to-buffer "*scratch*")
     (lisp-interaction-mode)
))

(global-set-key [(control f11)]
  '(lambda ()
     (interactive)
     (switch-to-buffer "*scratch*")
     (lisp-interaction-mode)
))

;;(global-set-key [f12] 'toggle-menubar-visible-p)
(global-set-key [f12] 'bookmark-jump)
(global-set-key [(shift f12)] 'bookmark-set)
(global-set-key [(control f12)] 'bookmark-save)
(global-set-key [(meta f12)] 'bookmark-delete)
(global-set-key [(meta f1)] 'find-include-etc-at-position)

(global-set-key "\M-a" 'apropos)
(global-unset-key "\C-z")
(define-key completion-list-mode-map "\C-f" 'other-window)
;;;;;;;;;;;;;; New keybindings
(global-set-key "\M-s" 'search-forward)
(global-set-key "\M-r" 'search-backward)
;(global-set-key "\C-@\C-s" 'my-search-forward-word-at-position)
;(global-set-key "\C-@\C-r" 'my-search-backward-word-at-position)

(global-set-key [(control ?\; )]
  '(lambda () (interactive)
     (dabbrev-expand -1)))

;(global-set-key [(control ?c) right]
;  'my-copy-line)

;(global-set-key [(control ?x) right]
;  'my-activate-line)
;(global-set-key "\C-c\C-l"
;  'my-activate-line)

(define-key isearch-mode-map "\C-k" 'isearch-edit-string)
(global-set-key "\C-c\C-]" 'eqn2eps)

(global-set-key "\C-c\C-]" 'eqn2eps-insert-filename)
(global-set-key "\C-xve" 'cvs-examine)
;(global-set-key "\M-e" 'find-tag)
;(global-set-key "\M-." 
;  '(lambda () (interactive) 
;     (let ((word (or (my-word-at-position) 
;		     (completing-read "tag: " tag-completion-table))))
;       (find-tag word))))
(global-set-key "\M-l" 'list-tags)

;(iswitchb-default-keybindings)
;(require 'iswitchb)
(defun my-list-buffers ()
  (interactive)
  (if (= (count-windows) 1)
      (split-window-vertically))
  (switch-to-buffer (list-buffers-noselect)))

(global-set-key "\C-x\C-b" 'list-buffers)
(global-set-key "\C-xb" 'list-buffers)
(global-set-key "\M-v" 'other-frame)
(global-set-key "\M-." 'find-tag-other-window)
;(global-set-key "\M-\\" 'migemo-toggle-isearch-enable)

(provide 'mykeymap)

;;; mykeymap.el ends here
