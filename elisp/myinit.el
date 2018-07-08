;; myinit.el		Created      : Thu Nov 27 17:30:57 2003
;;			Last modified: Sun Jul 08 07:13:18 2018
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

;(require 'tuareg)
(setq shell-command-switch "-ic")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(desktop-save-mode 1)
;(which-function-mode nil)
(setq desktop-restore-eager 3)
(setq max-lisp-eval-depth 10000)
(setq bookmark-save-flag 1)

(setq ring-bell-function 'ignore)
(load "ssh.el")

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup))

(add-hook 'tuareg-mode-hook #'merlin-mode)

;;; Mac-only configuration to use command and options keys
(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq mac-pass-command-to-system nil)

  (set-face-font 'default "Monaco-11")
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

(auto-compression-mode t)
(set-fringe-mode 1)
(setq calendar-week-start-day 1)

; utf8 
(set-language-environment "Japanese")

(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(load "myinsert.el")
(load "mymode.el")
(load "mykeymap.el")
(load "namazu.el")

;(load "edit-server.el")
;(edit-server-start)
;(setq edit-server-new-frame nil)

;;;;;;;;;;;;
;; namazu
(setq namazu-default-dir (expand-file-name "~/tmp/namazu"))

;;;;;;;;;;;;

(setq minibuffer-max-depth nil)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-step 1)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)
(setq help-char nil)

(display-time)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)
(setq display-time-string-forms
      '((format "%s/%s/%s(%s) %s:%s" year month day dayname 24-hours minutes)))

(exec-path-from-shell-initialize)

(line-number-mode 1)
(column-number-mode 1)

(setq enable-double-n-syntax t)

(setq enable-recursive-minibuffers nil)

(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
(require 'yaml-mode)
(setq time-stamp-line-limit 30)

;;(set-frame-height (selected-frame) 40)
;(setq comint-scroll-show-maximum-output t)

(setq grep-use-null-device nil)
;(ansi-color-for-comint-mode-on)

;; for emacs26
(defalias 'insert 'insert)

(require 'magit)
(global-set-key "\C-xg" 'magit-status)

(require 'flymake-python-pyflakes)
(setq flymake-python-pyflakes-executable "/usr/local/bin/flake8")

(custom-set-variables
 '(flymake-python-pyflakes-extra-arguments (quote ("--max-line-length=120"))))

(require 'ssh)
(setq ssh-directory-tracking-mode 'ftp)
(add-hook 'ssh-mode-hook
          '(lambda ()
            (shell-dirtrack-mode t)
            (setq dirtrackp nil)))

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
           '(lambda ()
              (flyspell-mode)))

(require 'wgrep)
(require 'wgrep-ag)

(provide 'myinit)
