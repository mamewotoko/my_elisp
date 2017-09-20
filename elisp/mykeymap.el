;;; mykeymap.el
;;                         Last modified: Wed Sep 20 19:17:03 2017

;; Author: Takashi Masuyama <mamewotoko@gmail.com>
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;
;;;キーバインドの変更

(global-set-key "\C-ci" 'my-insert-item-menu)
(global-set-key "\C-cy" 'my-increment-by-current-format-function)
(global-set-key "\C-cl" 'my-measure-length-of-region)

;;;ジャンプ
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-c\C-j" 'my-goto-error)

;;;置換
(global-set-key "\C-cr" 'query-replace)
;(global-set-key "\C-ct" 'insert-date-yy/mm/dd-format)
(global-set-key "\C-cf" 'insert-file-name)
(global-set-key "\C-cp" 'insert-file-name-prefix)

(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する

;;;正規表現で検索
(global-set-key "\C-c\C-r" 'search-backward-regexp)
(global-set-key "\C-c\C-s" 'search-forward-regexp)
(global-set-key "\C-c\C-c" 'comment-region)

(global-set-key [(super p)] '(lambda () (interactive)
                               (let ((ps-filename (read-input "ps filename: ")))
                                 (ps-print-buffer ps-filename))))

                                        ;(global-set-key "\C-x\C-f" 'helm-find-files)
(require 'magit)
(global-set-key "\C-xg" 'magit-status)

;;;ブックマークをつける
;;(global-set-key "\C-b" 'bookmark-set)
;;;ブックマークジャンプ
;;(global-set-key "\C-j" 'bookmark-jump)
;;;ブックマークを保存
;;(setq bookmark-save-flag 1)
(global-set-key (kbd "C-.") 'other-window)

(global-set-key "\C-t" 'copy-region-as-kill)

;(global-set-key "\C-c\C-t" 'my-increment-lambda-copy-region-register)
;(global-set-key "\C-x\C-y" 'my-increment-lambda-copy-yank)

(global-set-key "\C-o" 'dabbrev-expand)

(if (not (featurep 'kill-this-buffer))
    (defun kill-this-buffer ()
      (interactive)
      (kill-buffer (current-buffer))))

(global-set-key "\C-xk" 'kill-this-buffer)
;(global-set-key "\M-w" 'kill-word)
(global-set-key "\C-x\C-o" 'find-file-other-window)

(global-set-key [hiragana-katakana] 'toggle-input-method)
(global-set-key [(control tab)] 'next-buffer)
(global-set-key [(control shift tab)] 'previous-buffer)

(setq load-path (cons "/Users/tak/lib/emacs/elisp/dict" load-path))
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\C-h" 'backward-delete-char) ;; Ctrl+h  BSキー

(global-set-key "\M-g"
                '(lambda ()
                     (interactive)
                   (let ((pattern (read-from-minibuffer "pattern: ")))
                     (ag pattern default-directory))))

;;lisp
(global-set-key [f1] 'help-command)
(global-set-key [(control f1)] 'manual-entry)
(global-set-key [(meta f1)] 'apropos)

(global-set-key [f5] (lambda () (interactive) (progn (shell "*f5-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f6] (lambda () (interactive) (progn (shell "*f6-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f7] (lambda () (interactive) (progn (shell "*f7-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f8] (lambda () (interactive) (progn (shell) (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f9] (lambda () (interactive) (progn (shell "*f9-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))

(global-set-key [(control f5)] '(lambda () (interactive) (my-pushd-current-directory "*f5-shell*")))
(global-set-key [(control f6)] '(lambda () (interactive) (my-pushd-current-directory "*f6-shell*")))
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f7-shell*")))
(global-set-key [(control f8)] 'my-pushd-current-directory)
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f9-shell*")))

(require 'flymake)
(global-set-key (kbd "<M-up>") 'flymake-goto-previous-error)
(global-set-key (kbd "<M-down>") 'flymake-goto-next-error)

(define-key ctl-x-map [f8] 'ssh)
(global-set-key [(super h)] 'ignore)

(global-set-key "\C-xm" 'ignore)
(global-set-key [(control f9)] 'customize-apropos)
(global-set-key [(shift f9)] 'apropos-at-position)
(global-set-key [f10] 'namazu)
(global-set-key [(shift f10)] 
  (lambda () (interactive) 
    (let ((word (my-word-at-position)))
      (namazu 0 namazu-default-dir word))))

(global-set-key [zenkaku-hankaku] 'toggle-input-method)

;; (require 'helm-config)
;; (helm-mode 1)
;; (global-set-key "\M-y" 'helm-show-kill-ring)
;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;(define-key helm-M-x-map (kbd "TAB") 'helm-execute-persistent-action)

;(load "mynamazu.el")
;(global-set-key [(control f10)] 'set-namazu-dir-of-buffer-command)
;(global-set-key [(meta f10)] 'irchat)
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

(global-set-key [f12] 'bookmark-jump)
(global-set-key [(shift f12)] 'bookmark-set)
(global-set-key [(control f12)] 'bookmark-save)
(global-set-key [(meta f12)] 'bookmark-delete)
;(global-set-key [(meta f1)] 'find-include-etc-at-position)

(define-key global-map [?¥] [?\\]) 
;(global-set-key "\M-a" 'apropos)
(global-unset-key "\C-z")
;;;;;;;;;;;;;; New keybindings
(global-set-key [(control ?\; )]
  '(lambda () (interactive)
     (dabbrev-expand -1)))

(define-key isearch-mode-map "\C-k" 'isearch-edit-string)
;(global-set-key "\C-c\C-]" 'eqn2eps)
;(global-set-key "\C-c\C-]" 'eqn2eps-insert-filename)

(global-set-key "\M-l" 'list-tags)
(global-set-key "\C-x\C-b" 'list-buffers)
(global-set-key "\C-xb" 'list-buffers)
(global-set-key "\M-." 'find-tag-other-window)
(provide 'mykeymap)
;;; mykeymap.el ends here
