;;; mykeymap.el
;;                         Last modified: Wed May 09 20:53:06 2018

;; Author: Takashi Masuyama <mamewotoko@gmail.com>
;; Keywords: 

;;;;;;;;;;;;;;;;;;;;;;;
;;;キーバインドの変更

;(global-set-key "\C-ci" 'my-insert-item-menu)
;(global-set-key "\C-cy" 'my-increment-by-current-format-function)
(global-set-key "\C-cl" 'locate)

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
(global-set-key "\C-x\C-k" 'kill-this-buffer)

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

(defun my-pushd-current-directory (&optional buffer)
  (interactive)
  (let ((target-dir (expand-file-name ".")))
    (my-input-command-to-shell (concat "pushd " target-dir) buffer)))

;;lisp
(global-set-key [f1] 'help-command)
(global-set-key [(control f1)] 'manual-entry)
(global-set-key [(meta f1)] 'apropos)

(defun shell-or-ssh ()
  (interactive)
  (if (not buffer-file-name)
      (shell)
    (condition-case nil
      (tramp-file-name-user 
       (with-parsed-tramp-file-name buffer-file-name nil
         (if (not (tramp-file-name-p v))
             (shell)
           (let ((method (tramp-file-name-method v))
                 (user (tramp-file-name-user v))
                 (host (tramp-file-name-host v)))
             (message method)
             (if (string= method "scp")
                 (let ((connect (if user (format "%s@%s" user host)
                                  host)))
                   (ssh connect))
               (shell))))))
      (error (shell)))
  (set-buffer-process-coding-system 'utf-8 'utf-8)))

(global-set-key [f5] (lambda () (interactive) (progn (shell "*f5-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f6] (lambda () (interactive) (progn (shell "*f6-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f7] (lambda () (interactive) (progn (shell "*f7-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f8] 'shell-or-ssh)

(global-set-key [(control f5)] '(lambda () (interactive) (my-pushd-current-directory "*f5-shell*")))
(global-set-key [(control f6)] '(lambda () (interactive) (my-pushd-current-directory "*f6-shell*")))
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f7-shell*")))
(global-set-key [(control f8)] 'my-pushd-current-directory)
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f9-shell*")))

(define-key shell-mode-map "\C-p" 'comint-previous-input)
(define-key shell-mode-map "\C-n" 'comint-next-input)

(global-set-key [(shift f8)] '(lambda () (interactive) (ssh "deskvm")))
(global-set-key [(shift f7)] '(lambda () (interactive) (ssh "mamewo")))
(global-set-key [(shift f6)] '(lambda () (interactive) (ssh "google")))
(global-set-key [(shift f5)] '(lambda () (interactive) (ssh "vm")))

(require 'flymake)
(global-set-key (kbd "<M-up>") 'flymake-goto-previous-error)
(global-set-key (kbd "<M-down>") 'flymake-goto-next-error)

;(define-key ctl-x-map [f8] 'ssh)
(global-set-key [(super h)] 'ignore)

(global-set-key "\C-xm" 'ignore)
(global-set-key [f9] 'query-replace-regexp)
(global-set-key [(shift f9)] 'apropos)
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

(global-set-key [(shift f11)]
  '(lambda ()
     (interactive)
     (find-file "~/dev/diary/diary.org")
))

(global-set-key [f12] 'bookmark-bmenu-list)
(global-set-key [(control f12)] 'bookmark-set)
(global-set-key [(shift f12)] 'bookmark-save)
(global-set-key [(meta f12)] 'bookmark-delete)

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
(autoload 'ibuffer "ibuffer" "List buffers." t)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-xb" 'ibuffer)
(global-set-key [f2] 'ibuffer)
(global-set-key "\M-." 'find-tag-other-window)

(global-set-key "\M-f" 'forward-word)
(global-set-key "\M-b" 'backward-word)

(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?\" "w")
(modify-syntax-entry ?\\ "w")
(modify-syntax-entry ?- "w")

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe 
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
               (progn (comint-next-prompt 25535) (yank))
             (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
            nil
          (funcall pasteMe))
      (funcall pasteMe))
    ))

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w") (quote copy-word))

(defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
       ;;(paste-to-mark arg)
       )
(global-set-key (kbd "C-c l")         (quote copy-line))

(provide 'mykeymap)
;;; mykeymap.el ends here
