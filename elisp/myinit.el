;; myinit.el		Created      : Thu Nov 27 17:30:57 2003
;;			Last modified: Sun May 10 13:43:46 2020
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewotoko@gmail.com>

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/")
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize) ;; You might already have this line

(defvar my-favorite-packages
  '(
    ;;;; for auto-complete
    ag
    auto-complete
    company-lsp
    dockerfile-mode
    wgrep
    wgrep-ag
    ;zap-to-char
    yaml-mode
    terraform-mode
    svg
    monky
    merlin
    markdown-mode
    lsp-ui
    lsp-ocaml
    lsp-mode
    helm
    helm-ag-r
    csv
    git-link
    flycheck
    magit
    ))

(defvar my-install-package-p nil)
(if my-install-package-p
    (progn
      (package-refresh-contents)
      (dolist (package my-favorite-packages)
        (unless (package-installed-p package)
          (package-install package)))))

(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode 1))

(setq scroll-step 1)

(require 'epa-file)
(epa-file-enable)
(setq epg-gpg-program "/usr/local/bin/gpg2")

(require 'whitespace)
(whitespace-mode)

(setq dired-listing-switches "-alh")
;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(setq shell-command-switch "-c")
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

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

;(global-auto-complete-mode)
;(setq merlin-ac-setup 'easy)
;(add-hook 'caml-mode-hook 'merlin-mode)

(require 'lsp)
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(require 'doom-modeline)
(doom-modeline-mode 1)
;(load-theme 'doom-one t)

;;; Mac-only configuration to use command and options keys
(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-pass-command-to-system nil)

  (set-face-font 'default "Monaco-12")
  (set-face-attribute 'mode-line nil :font "Monaco-10")

  (set-background-color "#003300")
  (set-foreground-color "light gray")

  (set-face-font 'default "Monaco-20")
  
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
(setq my-font-size 12)
(set-face-font 'default (format "Monaco-%d" my-font-size))
(set-face-attribute 'mode-line nil :font (format "Monaco-%d" my-font-size))

(auto-compression-mode t)

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

;(exec-path-from-shell-initialize)

(line-number-mode 1)
(column-number-mode 1)

(setq enable-recursive-minibuffers nil)

;(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
;(require 'yaml-mode)
(setq time-stamp-line-limit 100)

(setq grep-use-null-device nil)
(ansi-color-for-comint-mode-on)

;; for emacs26
(defalias 'insert-string 'insert)
(defalias 'default-fill-column 'fill-column)

(global-flycheck-mode 1)

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
;;; myinit.el ends here

