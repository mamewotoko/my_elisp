;;; eqn2eps.el --- emacs interface for eqn2eps
;; Author: Katsuaki KAWACHI <kawachi@cim.pe.u-tokyo.ac.jp>
;; Edit: Takashi Masuyama <mamewotoko@gmail.com>
;; ver 0.1  : 03 Nov 1997
;; ver 0.11 : 06 Nov 1997
;; ver 0.12 : 11 Nov 1997
;; ver 0.2  : 02 Dec 1997
;; ver 0.3  : 25 Nov 2001
;
;
; ** インストール:
;
;  添付のファイル INSTALL の手順でインストールした後、
;  ~/.emacs に以下の記述を加える。
;
;     (autoload 'eqn2eps "eqn2eps" "equation to eps" t)
;     (autoload 'eqn2eps-region "eqn2eps" "equation to eps" t)
;
;  キーに割り当てる場合はさらに以下のような記述を加える。
;     (add-hook 'TeX-mode-hook
;          (function (lambda ()
;                      (local-set-key "\C-ce" 'eqn2eps)
;                      (local-set-key "\C-cr" 'eqn2eps-region)
;                      (local-set-key "\C-cm" 'eqn2eps-mode)
;                      (local-set-key "\C-cg" 'eqn2eps-set-mag))))
;
; ** 使い方
;
;   1. 現在のポインタを含む完全な数式を変換
;      → LaTeXの数式中にポインタを移動して M-x eqn2eps (C-c e)
;    
;   2. リージョン内の完全な数式をすべて変換
;      → \begin ... \end や $...$ 、\[...\]で完全にくくられた数式を
;        含む (複数数式可) リージョンを指定して M-x eqn2eps-region (C-c r)
;
;   3. ある数式の一部分を抜き出して変換
;      → \begin \end などの数式開始終了マークを *含まない* リージョンを
;        指定して M-x eqn2eps-region (C-c r)
;
;
; ** 設定変更
;
;   1. GIF/EPSI の切り替え
;     M-x eqn2eps-mode (C-c m) 
;   
;   2. 拡大率の設定
;     M-x eqn2eps-set-mag (C-c g)
;
;
; ** 初期設定変更 (~/.emacs に書いておく)
;
;   1. GIF を作るモードを初期設定とする
;     (setq eqn2eps-make-epsi nil)
;
;   2. 拡大率 num の初期値を設定する (拡大率 num は num/1000 倍を意味する)
;     (setq eqn2eps-mag "2000")   ; <- 200%
;
;   3. MS-Windows 上で利用するために、出力する EPS に tiff preview を付ける
;     (setq eqn2eps-add-tiff6-preview t)
;

(provide 'eqn2eps)

(defgroup eqn2eps nil
  "Equation to EPS converter."
  :prefix "eqn2eps-"
  :group 'tex)

(defcustom eqn2eps-make-epsi nil
  "EPS を出力フォーマットとするかどうか (nil なら出力が GIF になる)"
  :type 'boolean
  :group 'eqn2eps)

;;add by tak
(defcustom eqn2eps-make-jpeg t
  "EPS を出力フォーマットとするかどうか (nil なら出力が GIF になる)"
  :type 'boolean
  :group 'eqn2eps)
;;end

;(defcustom eqn2eps-mag "1000"
(defcustom eqn2eps-mag "500"
  "GIF を作るときの拡大率 (拡大率 num は num/1000 倍を意味する)"
  :type 'string
  :group 'eqn2eps)

(defcustom eqn2eps-add-tiff6-preview nil
  "EPS を出力するときに TIFF6 Preview を付加するかどうか"
  :type 'boolean
  :group 'eqn2eps)

;
(defconst eqn2eps-buffer "*eqn2eps output*")
(defconst eqn2eps-proc "eqn2eps")
(defconst eqn2eps-command "eqn2eps" "*Filename of eqn2eps command")
;
(defvar eqn2eps-resent-output-file-name nil)
(defvar eqn2eps-output-file-name-alist nil)
;
(defvar eqn2eps-bs "\\\\begin{equation}\\|\\\\begin{displaymath}\\|\\\\\\[\\|\\\\begin{eqnarray}\\|\\\\begin{eqnarray\\*}")
(defvar eqn2eps-es "\\\\end{equation}\\|\\\\end{displaymath}\\|\\\\\\]\\|\\\\end{eqnarray}\\|\\\\end{eqnarray\\*}")
;
(defun eqn2eps-mode (&optional x)
  "Toggle GIF or EPSI"
  (interactive)
  (if (called-interactively-p)
      (if eqn2eps-make-epsi
          (progn (setq eqn2eps-make-epsi nil)
                 (message "eqn2eps: GIF mode"))
        (setq eqn2eps-make-epsi t)
        (message "eqn2eps: EPSI mode"))
    (setq eqn2eps-make-epsi x)))

(defun eqn2eps-set-mag (&optional m)
  "Set magnification"
  (interactive)
  (if (called-interactively-p)
      (progn
        (setq m (read-from-minibuffer
                 "eqn2eps magnification (1000 means 100%) :" nil nil t nil))))
    (setq eqn2eps-mag (number-to-string m)))

(defun eqn2eps-region ()
  "Run eqn2eps on region"
  (interactive)
  (let ((eqstring (buffer-substring (region-beginning) (region-end))))
    (if (not (string-match (concat eqn2eps-bs "\\|\\$") eqstring))
        (setq eqstring (concat "\\[" eqstring "\\]")))
    (eqn2eps-exec eqstring (concat eqn2eps-command " on region:"))))
;
(defun eqn2eps ()
  "Run eqn2eps on pointing equation"
  (interactive)
  (let ((curpoint (point)))
    (re-search-backward (concat eqn2eps-bs "\\|\\$") nil nil 1)
    (push-mark nil nil t)
    (if (string-equal "$" (buffer-substring (point) (+ (point) 1)))
        ; inline equation
        (progn (re-search-forward "\\$" nil nil 2))
      ; display equation
      (re-search-forward eqn2eps-es nil nil 1)
      (forward-char 1))
    (eqn2eps-exec
     (buffer-substring (region-beginning) (region-end))
     (concat eqn2eps-command " on equation:"))
    (goto-char curpoint)))
;
(defun eqn2eps-exec (eqstring bmes)
  (if (eqn2eps-process-check)
      (let ((args (list "-i" "-x" eqn2eps-mag)))
	(if eqn2eps-make-jpeg (setq args (append args '("-j"))))
	(if (null eqn2eps-make-epsi) (setq args (append args '("-G"))))
	(if eqn2eps-add-tiff6-preview (setq args (append args '("-w"))))
	(setq args (append args '("-")))

        (setq bmes 
	      (concat bmes 
		      (if eqn2eps-make-epsi " EPSI" " GIF")
		      (if (and eqn2eps-make-epsi eqn2eps-add-tiff6-preview)
			  " (TIFF preview)")
		      " mode (mag=" eqn2eps-mag ")"))
        (message bmes)

	(apply 'start-process eqn2eps-proc eqn2eps-buffer eqn2eps-command args)
        (set-process-filter (get-process eqn2eps-proc) 'eqn2eps-filter)
        (process-send-string eqn2eps-proc eqstring)
        (process-send-string eqn2eps-proc "\n")
        (process-send-eof eqn2eps-proc))))

; adapted from shell-command-filter
(defun eqn2eps-filter (proc string)
  (if (string-match "^eqn2eps:" string)
      (progn
        (if (string-match "^eqn2eps:\\ generated" string)
	    ;;(sit-for 3 0 t))
	    (sit-for 3 0))
	(if (string-match "^eqn2eps:\\ \\(.+\\)\\ done" string)
	    (let ((ofile (match-string 1 string)))
	      (setq eqn2eps-resent-output-file-name ofile)
	      (setq eqn2eps-output-file-name-alist
		    (cons (list ofile)
			  eqn2eps-output-file-name-alist))))
	(message (substring string 0 -1))))
  (let* ((obuf (current-buffer))
         (buffer (process-buffer proc))
         opoint
         (window (get-buffer-window buffer))
         (pos (window-start window)))
    (unwind-protect
        (progn
          (set-buffer buffer)
          (or (= (point) (point-max))
              (setq opoint (point)))
          (goto-char (point-max))
          (insert-before-markers string))
      (set-window-start window pos)
      (if opoint
          (goto-char opoint))
      (goto-char (point-max))
      (set-buffer obuf))))

;(defun eqn2eps-buffer-clear (proc)
;  (let* ((obuf (current-buffer))
;	 (buffer (process-buffer proc)))
;    (unwind-protect
;	(progn
;	  (set-buffer buffer)
;          (erase-buffer)
;          (set-buffer obuf)))))

; adapted from auc-tex
(defun eqn2eps-process-check ()
  "Check if a process of eqn2ps already exist."
  (let ((process (get-process eqn2eps-proc)))
    (cond ((null process))
          ((not (eq (process-status process) 'run)))
          ((yes-or-no-p (concat "Process `" (process-name process)
                                " running, kill it? "))
           (delete-process process))
          (t (error "Cannot have two ean2ps processes")))))
    
