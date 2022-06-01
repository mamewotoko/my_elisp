;; myinit.el		Created      : Thu Nov 27 17:30:57 2003
;;			Last modified: Wed Jun 01 16:46:25 2022
;; Written by Takashi Masuyama <mamewotoko@gmail.com>
; install font used by doom
;M-x all-the-icons-install-fonts

(defvar my-install-package-p nil)
(setq byte-compile-warnings '(not cl-functions obsolete))

(setq load-path
      (append (list
               (expand-file-name "~/lib/emacs/elisp/opa/")
               ;; todo; submodule
               (expand-file-name "~/dev/ssh-el/")
               (expand-file-name "~/lib/emacs/elisp/ocaml/")
               (expand-file-name "~/lib/emacs/lisp/anthy"))
              load-path))

(if (>= emacs-major-version 26)
    (progn
      ;; for emacs26
      (defalias 'insert-string 'insert)
      (defalias 'default-fill-column 'fill-column)
      (defalias 'list-buffers 'ibuffer)
      ))

(setq load-path
      (append (list
               ;(expand-file-name "~/lib/emacs/elisp/opa/")
               ;; todo; submodule
               (expand-file-name "~/dev/ssh-el/")
               (expand-file-name "~/lib/emacs/elisp/ocaml/")
               (expand-file-name "~/lib/emacs/lisp/anthy")
               )
              load-path))

(require 'package)
(add-to-list 'package-archives
     '("melpa" . "http://melpa.org/packages/")
     '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(defalias 'list-buffers 'ibuffer)
(setq max-lisp-eval-depth 10000)
(auto-compression-mode t)

; for highlighting
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defvar my-favorite-packages
  '(
    ; for auto-complete
    ; lsp-mode
    ; lsp-ocaml
    ; lsp-ui
    ;zap-to-char
    ag
    auto-complete
    csv
    csv-mode
    csv-moode
    dockerfile-mode
    flycheck
    git-link
    helm
    helm-ag-r
    lsp-mode
    magit
    markdown-mode
    merlin
    monky
    svg
    terraform-mode
    tuareg
    wgrep
    wgrep-ag
    yaml-mode
;    company-lsp
;    lsp-ocaml
;    lsp-ui
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
;; (if (fboundp 'menu-bar-mode)
;;     (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'set-fringe-mode)
    (set-fringe-mode 1))

(setq scroll-step 1)
(setq max-lisp-eval-depth 10000)
(auto-compression-mode t)

;(global-flycheck-mode 1)
(global-auto-revert-mode nil)
(global-font-lock-mode t)
(setq use-dialog-box nil)

(setq dired-listing-switches "-alh")
;; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
(require 'dired)
(setq dired-dwim-target t)
;; ディレクトリを再帰的にコピーする
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)

(setq shell-command-switch "-c")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(desktop-save-mode 1)

(setq ring-bell-function 'ignore)

(require 'epa-file)
(epa-file-enable)
(setq epg-gpg-program "/usr/local/bin/gpg2")

(require 'whitespace)
;; https://emacs.stackexchange.com/questions/40622/disable-whitespace-mode-for-single-mode
(define-global-minor-mode my-global-whitespace-mode whitespace-mode
  (lambda ()
    (if (derived-mode-p 'markdown-mode
                        'python-mode
                        'elisp-mode
                        'tuareg-mode
                        'makefile-mode
                        'simple-mode)
      (whitespace-mode))))

(my-global-whitespace-mode 1)
;; indentにスペースを使う
(setq-default indent-tabs-mode nil)

; https://konbu13.hatenablog.com/entry/2014/04/01/203857
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        ))

;; skip warning
;(setq exec-path-from-shell-check-startup-files nil)

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

; Mac-only configuration to use command and options keys

(when (and (eq system-type 'darwin) (display-graphic-p))
  (setq my-font-size 18)
  (setq mac-pass-command-to-system nil)
  ;(exec-path-from-shell-initialize)
  (set-face-font 'default (format "Monaco-%d" my-font-size))
  (set-background-color "#003300")
  (set-foreground-color "light gray")
  ;(set-face-font 'default "Monaco-20")
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

;; right command as super key
(setq ns-right-command-modifier 'super)
(defun my-set-font-size (size mode-line-size)
  (interactive)
  (set-face-font 'default (format "Monaco-%d" size))
  (set-face-attribute 'mode-line nil :font (format "Monaco-%d" mode-line-size)))
;(my-set-font-size 16 14)

(auto-compression-mode t)

;; utf8
(set-language-environment "Japanese")

(if t
    (progn
      (load "myinsert.el")
      (load "mymode.el")
      (load "mykeymap.el"))
  (progn
    (require 'myinsert)
    (require 'mymode)
    (require 'mykeymap)))

(setq enable-recursive-minibuffers t)

(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)

;(exec-path-from-shell-initialize)

(require 'helm)
(require 'helm-config)
(helm-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-x h") 'helm-command-prefix)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)

(set-face-attribute 'linum nil :background "#aaa")
(set-face-attribute 'linum nil :foreground "#000")

;(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
;(require 'yaml-mode)
(setq time-stamp-line-limit 100)
(ansi-color-for-comint-mode-on)

;; for emacs26
(defalias 'insert-string 'insert)
(defalias 'default-fill-column 'fill-column)

(global-flycheck-mode 1)

(progn
  (set-face-background 'mode-line "gray30")
  (set-face-background 'mode-line-inactive "black"))

(doom-modeline-mode 1)
(setq doom-modeline-height 0)
(setq doom-modeline-vcs-max-length 20)
(setq minibuffer-max-depth nil)

(setq time-stamp-line-limit 100)
(setq grep-use-null-device nil)
(ansi-color-for-comint-mode-on)

(require 'markdown-mode)
(add-hook 'markdown-mode-hook
          '(lambda ()
             (flyspell-mode)))

(setq calendar-week-start-day 1)
(eval-after-load "holidays"
  '(progn
     ;; week starts from monday
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
; agした結果のbuffer上で編集
(require 'wgrep-ag)
(provide 'myinit)
;;; myinit.el ends here
