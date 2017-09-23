;; myinit.el		Created      : Thu Nov 27 17:30:57 2003
;;			Last modified: 土曜日 9月 23 08:44:28 2017
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs ;;

(require 'tuareg)
(setq shell-command-switch "-ic")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(desktop-save-mode 1)
;(which-function-mode nil)
(setq desktop-restore-eager 3)
(setq max-lisp-eval-depth 10000)

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize) ;; You might already have this line
(setq ring-bell-function 'ignore)

;;; Mac-only configuration to use command and options keys
(when (eq system-type 'darwin)
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

; utf8 
(set-language-environment "Japanese")

(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(load "myinsert.el")
(load "mymode.el")
(load "mykeymap.el")

(setq minibuffer-max-depth nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq scroll-step 1)

;; (require 'helm)
;; (require 'helm-config)

;; ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;; (global-set-key (kbd "C-c h") 'helm-command-prefix)
;; (global-unset-key (kbd "C-x c"))

;(setq default-input-method "MacOSX")
;(setq default-input-method "japanese")
;;(global-set-key [(control ?¥)] 'toggle-input-method)
;;(global-set-key [?\]\\
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)
(setq help-char nil)

(require 'wgrep)
(require 'wgrep-ag)

(display-time)
(setq display-time-day-and-date t)
(setq display-time-default-load-average nil)

(exec-path-from-shell-initialize)
;(load-file "~/lib/emacs/elisp/opa/emacs_conf.el")
;(load-file "~/lib/emacs/elisp/opa/opa-mode.el")

(line-number-mode 1)
(column-number-mode 1)

(setq enable-double-n-syntax t)

;; (autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
;; (mwheel-install)

(setq enable-recursive-minibuffers nil)

(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
;(load "grep-edit.el")
(load "yaml-mode.el")
(setq time-stamp-line-limit 30)

;;(set-frame-height (selected-frame) 40)
;(setq comint-scroll-show-maximum-output t)

(setq grep-use-null-device nil)
;(ansi-color-for-comint-mode-on)

(provide 'myinit)
