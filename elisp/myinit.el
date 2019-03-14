;; myinit.el		Created      : Thu Nov 27 17:30:57 2003
;;			Last modified: 金曜日 3月 15 08:22:02 2019
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs ;;

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize) ;; You might already have this line

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-step 1)

;;; sdic
(setq load-path (cons (expand-file-name "~/lib/emacs/elisp/sdic-2.1.3/lisp") load-path))
(autoload 'sdic-describe-word "sdic" "英単語の意味を調べる" t nil)
(global-set-key "\C-cw" 'sdic-describe-word)
(autoload 'sdic-describe-word-at-point "sdic" "カーソルの位置の英単語の意味を調べる" t nil)
(global-set-key "\C-cW" 'sdic-describe-word-at-point)

(require 'whitespace)
(whitespace-mode)

(setq dired-listing-switches "-alh")

(setq shell-command-switch "-ic")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(desktop-save-mode 1)

(setq desktop-restore-eager 3)
(setq max-lisp-eval-depth 10000)
(setq bookmark-save-flag 1)

(setq ring-bell-function 'ignore)

;; skip warning
(setq exec-path-from-shell-check-startup-files nil)

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)

;;; Mac-only configuration to use command and options keys
(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-pass-command-to-system nil)

  (set-face-font 'default "Monaco-13")
  (set-face-attribute 'mode-line nil :font "Monaco-11")
  (set-background-color "#003300")
  (set-foreground-color "light gray")

  ;; Mac-only
  ;; Command key as Meta key, Option key untouched
  ;; http://www.emacswiki.org/emacs/MetaKeyProblems#toc15
  ;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
  ;;
  ;; left command
  (setq mac-command-modifier 'meta)
  ;; left option
  ;(setq mac-option-modifier 'alt)
  ;;xb
  ;; right command
  (setq mac-right-command-modifier 'super)
  ;; right option
  ;(setq mac-right-option-modifier 'hyper)
  ;;
  ;; Mac Binding modifier keys
  ;; http://www.emacswiki.org/emacs/EmacsForMacOS#toc23
  ;; mac-function-modifier
  ;; mac-control-modifier
  ;; mac-command-modifier
  ;; mac-option-modifier
  ;; mac-right-command-modifier
  ;; mac-right-control-modifier
  ;; mac-right-option-modifier
  ;; values can be 'control (C), 'alt (A), 'meta (M), 'super (s), or 'hyper (H).
  ;; setting to nil allows the OS to assign values
)
; (set-face-font 'default "Monaco-13")
;  (set-face-attribute 'mode-line nil :font "Monaco-11")

(auto-compression-mode t)
(set-fringe-mode 1)

;; utf8
(set-language-environment "Japanese")

(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load "myinsert.el")
(load "mymode.el")
(load "mykeymap.el")

(setq minibuffer-max-depth nil)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)
(setq help-char nil)

;(display-time)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(when (window-system)
  (setq display-time-string-forms
        '((format "%s/%s/%s(%s) %s:%s" year month day dayname 24-hours minutes)))
  (display-time)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq frame-title-format '("" global-mode-string
                             (:eval (if (buffer-file-name) " %f" " %b")))))

;(exec-path-from-shell-initialize)

(require 'helm-config)
(helm-mode 1)
(require 'helm)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-x h") 'helm-command-prefix)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab 
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(line-number-mode 1)
(column-number-mode 1)

(setq enable-recursive-minibuffers nil)

(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
;(require 'yaml-mode)
(setq time-stamp-line-limit 100)

(setq grep-use-null-device nil)
(ansi-color-for-comint-mode-on)

;; for emacs26
(defalias 'insert-string 'insert)
(defalias 'default-fill-column 'fill-column)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(global-flycheck-mode 1)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "~/Music/")
; Play FLAC with flac123.
(define-emms-simple-player flac123 '(file) 
  "\\.flac$" "/usr/local/bin/flac123")
(add-to-list 'emms-player-list emms-player-flac123)

; Add music file or directory to EMMS playlist on ! in dired.
(define-key dired-mode-map "!" 'emms-add-dired)
(define-emms-simple-player afplay '(file)
  (regexp-opt '(".mp3" ".m4a" ".aac"))
  "afplay")
(setq emms-player-list `(,emms-player-afplay))

(global-set-key [(shift f1)] 'emms-metaplaylist-mode-go)

;; customized
(load "ssh.el")
;(require 'ssh)
(setq ssh-directory-tracking-mode 'ftp)
(add-hook 'ssh-mode-hook
          '(lambda ()
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
           '(lambda ()
              (flyspell-mode)))

(setq calendar-week-start-day 1)
(eval-after-load "holidays"
  '(progn
     (require 'japanese-holidays)
     (setq calendar-holidays ; 他の国の祝日も表示させたい場合は適当に調整
           (append japanese-holidays holiday-local-holidays holiday-other-holidays))
     (setq mark-holidays-in-calendar t) ; 祝日をカレンダーに表示
     ;; 土曜日・日曜日を祝日として表示する場合、以下の設定を追加します。
     ;; 変数はデフォルトで設定済み
     (setq japanese-holiday-weekend '(0 6)     ; 土日を祝日として表示
           japanese-holiday-weekend-marker     ; 土曜日を水色で表示
           '(holiday nil nil nil nil nil japanese-holiday-saturday))
     (add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
     (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
     ;; “きょう”をマークするには以下の設定を追加します。
     (add-hook 'calendar-today-visible-hook 'calendar-mark-today)))

(require 'wgrep)
(require 'wgrep-ag)

(provide 'myinit)
