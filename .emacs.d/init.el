(setq load-path
      (append (list (expand-file-name "~/lib/emacs/elisp/")
		    (expand-file-name "~/lib/emacs/elisp/opa/")
		    (expand-file-name "~/lib/emacs/elisp/ocaml/")
		    (expand-file-name "~/lib/emacs/lisp/anthy"))
		    load-path))

(defalias 'list-buffers 'ibuffer)
(tool-bar-mode nil)

(setq max-lisp-eval-depth 10000)
(if nil    ; xemacs code
  (progn 
    (set-specifier menubar-visible-p nil) 
    (set-specifier top-toolbar-visible-p nil) 
    (set-specifier default-gutter-visible-p nil)))

;; (set-face-font 'default "-misc-fixed-medium-r-normal-*-12-*-*-*-*-*-*-*")
;; (set-face-font 'default "-apple-osaka-medium-*-*-*-12-*-*-*-*-*-*-*")
;(set-face-font 'default "-sony-fixed-medium-*-*-*-11-*-*-*-*-*-*-*")

;
;======================================================================
; Anthy
;======================================================================
;(load-library "anthy")
;; (load "anthy.el")
;; (load "leim-list.el")
;; (setq default-input-method "japanese-anthy")
;(setq anthy-default-enable-enum-candidate-p nil)

(auto-compression-mode t)	

; xemacs code
;(set-face-background 'default "lavender" nil '(x color))
(set-face-background 'default "#F0FFF0")

;(autoload 'lookup "lookup" nil t)
;(autoload 'lookup-region "lookup" nil t)
;(autoload 'lookup-pattern "lookup" nil t)
;(define-key ctl-x-map "l" 'lookup)              ; C-x l - lookup
;(define-key ctl-x-map "y" 'lookup-region)       ; C-x y - lookup-region
;(define-key ctl-x-map "\C-y" 'lookup-pattern)   ; C-x C-y - lookup-pattern

;(setq auto-mode-alist
;  (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

; utf8 
(set-language-environment "Japanese")

; xemacs code
;(when (equal emacs-major-version 21) (require 'un-define))
;(unless (emacs-version>= 21 5 6)
;  (require 'mule-ucs-unicode "unicode"))

;(set-buffer-file-coding-system 'utf-8)
;(setq default-buffer-file-coding-system 'utf-8)
;(setq file-name-coding-system 'utf-8);

(setq auto-mode-alist
          (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

; for highlighting

(if window-system (require 'caml-font))

(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
;(setq process-coding-system-alist '((".*bash$" 'utf-8)))
;(set-coding-category-system 'utf-8 'utf-8)
;(set-coding-priority-list '(utf-8))


;;;(require 'advice)
(load "myinsert.el")
(load "mymode.el")
(load "mykeymap.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'myinsert)
;; (require 'mymode)
;; (require 'mykeymap)

;(setq gnus-nntp-server nil)
;(setq gnus-select-method '(nntp "news01.so-net.ne.jp"))

(setq minibuffer-max-depth nil)

(scroll-bar-mode nil)
(setq scroll-step 1)

(global-set-key [(control ?¥)] 'toggle-input-method)
(global-set-key "\C-h" 'backward-delete-char)
(global-set-key [f1] 'help-command)

(display-time)
(setq display-time-day-and-date t)

(line-number-mode 1)
(column-number-mode 1)

(setq enable-double-n-syntax t)

;(autoload 'mew "mew" nil t)
;(autoload 'mew-send "mew" nil t)
;(setq mew-use-cached-passwd t)
;(setq mew-mail-domain-list '("okuiaki.com"))
;(setq mew-icon-directory "")
;(setq mew-cc "mamewo@dk9.so-net.ne.jp")
;(setq mew-fcc "+Sent")
;(autoload 'mew-user-agent-compose "mew" nil t)
;(if (boundp 'mail-user-agent)
;    (setq mail-user-agent 'mew-user-agent))
;(if (fboundp 'define-mail-user-agent)
;    (define-mail-user-agent
;      'mew-user-agent
;      'mew-user-agent-compose
;      'mew-draft-send-letter
;      'mew-draft-kill
;      'mew-send-hook))

;(setq mew-refile-guess-control
;      '(mew-refile-guess-by-alist
;	mew-refile-ctrl-auto-boundary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YaTeX
;;
;(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
;(setq YaTeX-help-file (expand-file-name "~/lib/emacs/yatex/YATEXHLP.jp"))

;(setq edict-dictionaries (list (expand-file-name "~/lib/edict/edict")))

;(define-key global-map 'yen [?\\])

(autoload 'mwheel-install "mwheel" "Enable mouse wheel support.")
(mwheel-install)

(setq enable-recursive-minibuffers t)

; xemacs code
;(require 'pc-select)
;(pc-select-mode 1)
(pc-selection-mode)

;;(require 'namazu)
;(autoload 'namazu "namazu" nil t)
;(setq namazu-dir-alist
;      '(("Jsource" . "/home/tak/.java_namazu/from_source")
;	("JAPI" . "/home/tak/.java_namazu")
;	("MLManual" . "/home/tak/.ocaml_namazu")))
;(require 'mygoogle)

;; (load "yasima.el")
;; (yasima-mode t)

(add-to-list 'load-path (expand-file-name "~/lib/emacs/elisp/scala-mode"))
(load "scala-mode-auto.el")

(autoload 'opa-mode (expand-file-name "~/lib/emacs/elisp/opa/opa-mode.el") "OPA editing mode." t)
(add-to-list 'auto-mode-alist '("\\.opa$" . opa-mode))
(load "grep-edit.el")
(setq time-stamp-line-limit 30)

;;(set-frame-height (selected-frame) 40)

;(ansi-color-for-comint-mode-on)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(display-time-24hr-format t)
 '(grep-command "grep -nHI -r -e  *")
 '(initial-buffer-choice t)
 '(ns-command-modifier (quote meta)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(cursor ((default nil) (nil (:background "aqua" :foreground "aqua"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "khaki" :foreground "black" :box (:line-width -1 :style released-button))))))
(put 'narrow-to-region 'disabled nil)

(put 'erase-buffer 'disabled nil)
