;;;　mykeymap.el --- keybindings
;;; Commentary:
;; Author: Takashi Masuyama <mamewotoko@gmail.com>
;; Keywords:

;;; Code:

(global-set-key (kbd "C-\;") 'completion-at-point)
(global-set-key [(control ?¥)] 'toggle-input-method)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)

;(global-set-key "\C-cl" 'locate)

;;;ジャンプ
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-:") 'goto-line)
(global-set-key (kbd "C-c C-j") 'my-goto-error)

(global-set-key (kbd "C-c g l") 'git-link)
(global-set-key (kbd "C-c g c") 'git-link-commit)

;;;置換
(global-set-key "\C-cr" 'query-replace)

(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する

(global-set-key [(super p)] '(lambda () (interactive)
                               (let ((ps-filename (read-input "ps filename: ")))
                                 (ps-print-buffer ps-filename))))

(global-set-key (kbd "C-.") 'other-window)
(global-set-key "\C-t" 'copy-region-as-kill)

(require 'helm-config)
(helm-mode 1)
(require 'helm)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-x h") 'helm-command-prefix)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-/") 'helm-find)
(setq helm-ff-skip-boring-files t)

;(global-set-key "\C-o" 'dabbrev-expand)
(global-set-key "\C-o" 'helm-dabbrev)

(if (not (featurep 'kill-this-buffer))
    (defun kill-this-buffer ()
      (interactive)
      (kill-buffer (current-buffer))))

(global-set-key "\C-xk" 'kill-this-buffer)
;(global-set-key "\C-x\C-k" 'kill-this-buffer)
(global-set-key "\C-x\C-o" 'find-file-other-window)

(global-set-key [hiragana-katakana] 'toggle-input-method)
(global-set-key [(control tab)] 'next-buffer)
(global-set-key [(control shift tab)] 'previous-buffer)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key "\C-h" 'backward-delete-char)

(global-set-key "\M-g"
                '(lambda ()
                     (interactive)
                   (let ((pattern (read-from-minibuffer "pattern: ")))
                     (ag pattern default-directory))))
(global-set-key "\M-G" 'ag-files)

(push (cons "\\*.*shell\\*" display-buffer--same-window-action) display-buffer-alist)

(defun my-pushd-current-directory (&optional buffer)
  (interactive)
  (let ((target-dir (expand-file-name ".")))
    (my-input-command-to-shell (concat (format "pushd '%s'" target-dir)) buffer)))

(global-set-key "\C-xg" 'magit-status)

;;lisp
(global-set-key [f1] 'help-command)
(global-set-key [(control f1)] 'manual-entry)
(global-set-key [(meta f1)] 'helm-apropos)
(define-key help-map "a" 'apropos)

;;; older emacs: version xxxx
;;; use ssh.el
(defun shell-or-ssh-old ()
  (interactive)
  (if (not buffer-file-name)
      (shell)
    (condition-case nil
      (tramp-file-name-user
       (with-parsed-tramp-file-name buffer-file-name nil
         ;; v is defined in with-parsed-tramp-file-name macro
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
      (error (shell))))
    (set-buffer-process-coding-system 'utf-8 'utf-8))

(require 'tramp)

;;; use shell
(defun shell-or-ssh ()
  (interactive)
  (if (not buffer-file-name)
      (shell)
    (if (or (string-prefix-p "/scp:" buffer-file-name)
            (string-prefix-p "/ssh:" buffer-file-name))
        (let* ((connect (if tramp-current-user (format "%s@%s" tramp-current-user tramp-current-host)
                          tramp-current-host))
               (bufname (format "*shell %s*" connect)))
          (shell bufname))
      (shell)))
  (set-buffer-process-coding-system 'utf-8 'utf-8))

(global-set-key [f5] (lambda () (interactive) (progn (shell "*f5-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f6] (lambda () (interactive) (progn (shell "*f6-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f7] (lambda () (interactive) (progn (shell "*f7-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f8] 'shell-or-ssh)

(defun my-input-command-to-shell (command &optional buffer)
  (if buffer
      (shell buffer)
    (shell))
  (goto-char (point-max))
  (comint-kill-whole-line 1)
  (insert command)
  (comint-send-input)
  (comint-next-prompt 1))

(defun my-pushd-current-directory (&optional buffer)
  (interactive)
  (let ((target-dir (expand-file-name ".")))
    (if (> (count-windows) 1)
	(other-window 1)
      (split-window-vertically))
    (my-input-command-to-shell (concat "pushd " target-dir) buffer)))

(global-set-key [(control f5)] '(lambda () (interactive) (my-pushd-current-directory "*f5-shell*")))
(global-set-key [(control f6)] '(lambda () (interactive) (my-pushd-current-directory "*f6-shell*")))
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f7-shell*")))
(global-set-key [(control f8)] 'my-pushd-current-directory)
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f9-shell*")))

(define-key shell-mode-map "\C-p" 'comint-previous-input)
(define-key shell-mode-map "\C-n" 'comint-next-input)

(global-set-key [(shift f8)] '(lambda () (interactive) (ssh "mamewo")))
(global-set-key [(shift f7)] '(lambda () (interactive) (ssh "deskvm")))
(global-set-key [(shift f6)] '(lambda () (interactive) (ssh "google")))
(global-set-key [(shift f5)] '(lambda () (interactive) (ssh "vm")))

;(require 'flycheck)
;(global-set-key (kbd "<M-up>") 'flycheck-previous-error)
;(global-set-key (kbd "<M-down>") 'flycheck-next-error)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-x\C-f" 'helm-find-files)
;; For find-file etc.
(require 'helm-files)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
;; For helm-find-files etc.
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

;(define-key ctl-x-map [f8] 'ssh)
(global-set-key [(super h)] 'ignore)

(global-set-key "\C-xm" 'ignore)
(global-set-key [f9] 'query-replace-regexp)
(global-set-key [(shift f9)] 'apropos)
(global-set-key [(ctrl f9)] 'helm-apropos)
(global-set-key [f10] 'namazu)
(global-set-key [zenkaku-hankaku] 'toggle-input-method)

                                        ;(require 'zop-to-char)
(global-set-key (kbd "M-z") 'zop-to-char)

(global-set-key [f11]
  '(lambda ()
     (interactive)
     (switch-to-buffer "*scratch*")
     (lisp-interaction-mode)
))
(global-set-key [(shift f11)] 'calendar)
;; (global-set-key [(control f11)]
;;   '(lambda ()
;;      (interactive)
;;      (find-file "~/dev/diary/diary.md")
;; ))
(global-set-key [(meta f11)] '(lambda () (interactive) (my-set-font-size 24)))
(global-set-key [(shift meta f11)] '(lambda () (interactive) (my-set-font-size 12)))

; private
;; (load "aquos.el")
;; (global-set-key [(meta f11)] 'aquos-remocon)

(global-set-key [f12] 'helm-bookmarks)
(global-set-key [(control f12)] 'bookmark-set)
(global-set-key [(shift f12)] 'bookmark-save)
(global-set-key [(meta f12)] 'bookmark-delete)

(define-key global-map [?¥] [?\\])
(global-unset-key "\C-z")
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;;;;;;;;;;;;; New keybindings

(define-key isearch-mode-map "\C-k" 'isearch-edit-string)

(global-set-key "\M-l" 'list-tags)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-xb" 'switch-to-buffer)
(global-set-key [f2] 'helm-imenu)
(global-set-key [(control f2)] 'helm-mini)
(global-set-key "\M-." 'xref-find-definitions-other-window)
                ;find-tag-other-window)

;(global-set-key "\M-f" 'forward-word)
                                        ;(global-set-key "\M-b" 'backward-word)

(global-set-key "\M-f" 'forward-whitespace)
(global-set-key "\M-b" (lambda () (interactive) (forward-whitespace -1)))

(global-set-key "\M-o" 'helm-occur)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h g") 'helm-ag)

;(global-set-key "\M-o" 'helm-occur)

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

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

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
  ;(paste-to-mark arg)
  )

(defun back-to-space (&optional arg)
  (interactive "P")
  (skip-syntax-backward "^ ()\""))

(defun forward-to-space (&optional arg)
  (interactive "P")
  (skip-syntax-forward "^ ()\""))

(defun copy-non-space (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'back-to-space 'forward-to-space arg)
  ;(paste-to-mark arg)
  )

(global-set-key (kbd "C-c w") (quote copy-word))
(global-set-key (kbd "C-c t") (quote copy-non-space))

(defun copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line "
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  ;;(paste-to-mark arg)
  )
(global-set-key (kbd "C-c l") (quote copy-line))

(provide 'mykeymap)

;;; mykeymap.el ends here
