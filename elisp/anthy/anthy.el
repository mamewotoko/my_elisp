;;; anthy.el -- Anthy

;; Copyright (C) 2001 - 2003 KMC(Kyoto University Micro Computer Club)

;; Author: Yusuke Tabata<yusuke@kmc.gr.jp>
;;         Tomoharu Ugawa
;; Keywords: japanese

;; This file is part of Anthy

;;; Commentary:
;;
;; かな漢字変換エンジン Anthyを emacsから使うためのプログラム
;; Anthyライブラリを使うためのコマンドanthy-agentを起動して、
;; anthy-agentとパイプで通信をすることによって変換の動作を行う
;;
;;
;; Funded by IPA未踏ソフトウェア創造事業 2001 10/10
;;
;; 開発はemacs21.2上で行っていてminor-mode
;; もしくはleimとしても使用できる
;; (set-input-method 'japanese-anthy)
;;
;; emacs19(mule),20,21で動作する
;; xemacsの場合は候補選択モードでキーを押すところにバグがある
;;
;;
;;
;; 2001-11-16 EUC-JP -> ISO-2022-JP
;;
;; TODO
;;  minibuffferの扱い
;;  isearch対応
;;
;; 用語
;;  commit 文字列を確定すること
;;  preedit(プリエディット) 確定前の文字列アンダーラインや強調の属性も含む
;;  segment(文節) 文法的な文節ではなく，同じ属性の文字列のかたまり
;;

;;; Code:
;(setq debug-on-error t)

(defvar anthy-default-enable-enum-candidate-p t
  "これを設定すると次候補を数回押した際に候補の一覧から選択するモードになります．")
(defvar anthy-personality ""
  "パーソナリティ")

(defconst anthy-working-buffer " *anthy*")
(defvar anthy-agent-process nil
  "anthy-agentのプロセス")
;;
(defvar anthy-agent-command-list '("anthy-agent")
  "anthy-agentのPATH名")

;; face
(defvar anthy-hilight-face nil)
(defvar anthy-underline-face nil)
(copy-face 'highlight 'anthy-highlight-face)
(set-face-underline-p 'anthy-highlight-face t)
(copy-face 'underline 'anthy-underline-face)

;;
(defvar anthy-xemacs
  (if (featurep 'xemacs)
      t nil))
(if anthy-xemacs
    (require 'overlay))
;;
(defvar anthy-mode-map nil
  "AnthyのASCIIモードのキーマップ")
(or anthy-mode-map
    (let ((map (make-keymap))
	  (i 32))
      (define-key map (char-to-string 10) 'anthy-insert)
      (while (< i 127)
	(define-key map (char-to-string i) 'anthy-insert)
	(setq i (+ 1 i)))
      (setq anthy-mode-map map)))
;;
(defvar anthy-preedit-keymap nil
  "Anthyのpreeditのキーマップ")
(or anthy-preedit-keymap
    (let ((map (make-keymap))
	  (i 0))
      ;; 通常の文字に対して
      (while (< i 128)
	(define-key map (char-to-string i) 'anthy-insert)
	(setq i (+ 1 i)))
      ;; バックスペース
      (substitute-key-definition
       'delete-backward-char 'anthy-insert-backspace map global-map)
      ;; 文節の伸縮
      (define-key map [(shift left)] 'anthy-insert)
      (define-key map [(shift right)] 'anthy-insert)
      ;; emacsの種類に応じて左右の矢印キー対する動作を設定する
      (if anthy-xemacs
	  (progn
	    (substitute-key-definition
	     'backward-char-command 'anthy-insert-left map global-map)
	    (substitute-key-definition
	     'forward-char-command 'anthy-insert-right map global-map))
	(progn
	  (substitute-key-definition
	   'backward-char 'anthy-insert-left map global-map)
	  (substitute-key-definition
	   'forward-char 'anthy-insert-right map global-map)))
      (setq anthy-preedit-keymap map)))

;; anthy-agentに送る際にキーをエンコードするためのテーブル
(defvar anthy-keyencode-alist
  '((1 . "(ctrl A)") ;; \C-a
    (2 . "(left)") ;; \C-b
    (4 . "(ctrl D)") ;; \C-d
    (5 . "(ctrl E)") ;; \C-e
    (6 . "(right)") ;; \C-f
    (7 . "(esc)") ;; \C-g
    (8 . "(ctrl H)") ;; \C-h
    (9 . "(shift left)") ;; \C-i
    (10 . "(ctrl J)")
    (11 . "(ctrl K)")
    (13 . "(enter)") ;; \C-m
    (14 . "(space)") ;; \C-n
    (15 . "(shift right)") ;; \C-o
    (16 . "(up)") ;; \C-p
    (32 . "(space)")
    (40 . "(opar)") ;; '('
    (41 . "(cpar)") ;; ')'
    (127 . "(ctrl H)")
    ;; emacs map
    (S-right . "(shift right)")
    (S-left . "(shift left)")
    (right . "(right)")
    (left . "(left)")
    (up . "(up)")
    ;; xemacs
    ((shift right) . "(shift right)")
    ((shift left) . "(shift left)")
    ((right) . "(right)")
    ((left) . "(left)")
    ((up) . "(up)")))

;; モードラインの文字列
(defvar anthy-mode-line-string-alist
  '(("hiragana" . " あ")
    ("katakana" . " ア")
    ("alphabet" . " A")
    ("walphabet" . " Ａ")))

;; 最後に割り当てたcontext id
(defvar anthy-last-context-id 1)

;; From skk-macs.el From viper-util.el.  Welcome!
(defmacro anthy-deflocalvar (var default-value &optional documentation)
  (` (progn
       (defvar (, var) (, default-value)
	 (, (format "%s\n\(buffer local\)" documentation)))
       (make-variable-buffer-local '(, var))
       )))

;; buffer local variables
(anthy-deflocalvar anthy-context-id nil)
; モードの管理
(anthy-deflocalvar anthy-minor-mode nil)
(anthy-deflocalvar anthy-mode nil)
(anthy-deflocalvar anthy-leim-active-p nil)
(anthy-deflocalvar anthy-saved-mode nil)
; プリエディット
(anthy-deflocalvar anthy-preedit "")
(anthy-deflocalvar anthy-preedit-start 0)
(anthy-deflocalvar anthy-preedit-overlays '())
(anthy-deflocalvar anthy-mode-line-string " A")
; 候補列挙
(anthy-deflocalvar anthy-enum-candidate-p nil)
(anthy-deflocalvar anthy-enum-rcandidate-p nil)
(anthy-deflocalvar anthy-candidate-minibuffer "")
(anthy-deflocalvar anthy-enum-candidate-list '()
		   "((画面内のindex 候補のindex . 候補文字列) ..)")
(anthy-deflocalvar anthy-enable-enum-candidate-p 
  (cons anthy-default-enable-enum-candidate-p nil)
  "このバッファで候補の列挙を行うかどうか")

; 入力状態
(anthy-deflocalvar anthy-current-rkmap "hiragana")
;;
(defvar anthy-wide-space "　")

;;; setup minor-mode
;; minor-mode-alist
(if (not
     (assq 'anthy-minor-mode minor-mode-alist))
    (setq minor-mode-alist
       (cons
	(cons 'anthy-minor-mode '(anthy-mode-line-string))
	minor-mode-alist)))
;; minor-mode-map-alist
(if (not
     (assq 'anthy-minor-mode minor-mode-map-alist))
    (setq minor-mode-map-alist
       (cons
	(cons 'anthy-minor-mode anthy-mode-map)
	minor-mode-map-alist)))

;;
(defun anthy-process-sentinel (proc stat)
  "プロセスの状態が変化したら参照を消して，次に再起動できるようにする"
  (message "%s" stat)
  (setq anthy-agent-process nil))

;;; status
(defun anthy-update-mode-line ()
  "モードラインを更新する"
  (let ((a (assoc anthy-current-rkmap anthy-mode-line-string-alist)))
    (if a
	(progn
	 (setq anthy-mode-line-string (cdr a))
	 (setq current-input-method-title
	       (concat "<Anthy:" (cdr a) ">")))))
  (force-mode-line-update))

;;; preedit control
(defun anthy-erase-preedit ()
  "プリエディットを全部消す"
  (if (> (string-width anthy-preedit) 0)
      (let* ((str anthy-preedit)
	     (len (length str))
	     (start anthy-preedit-start))
	(delete-region start (+ start len))
	(goto-char start)))
  (setq anthy-preedit "")
  (mapcar 'delete-overlay anthy-preedit-overlays)
  (setq anthy-preedit-overlays nil))

(defun anthy-select-face-by-attr (attr)
  "文節の属性に応じたfaceを返す"
  (if (memq 'RV attr)
      'anthy-highlight-face
    'anthy-underline-face))

(defun anthy-enable-preedit-keymap ()
  "キーマップをプリエディットの存在する時のものに切替える"
  (setcdr
   (assq 'anthy-minor-mode minor-mode-map-alist)
   anthy-preedit-keymap))

(defun anthy-disable-preedit-keymap ()
  "キーマップをプリエディットの存在しない時のものに切替える"
  (setcdr
   (assq 'anthy-minor-mode minor-mode-map-alist)
   anthy-mode-map)
  (anthy-update-mode-line))

(defun anthy-insert-preedit-segment (str attr)
  "プリエディットを一文節文追加する"
  (let ((start (point))
	(end) (ol))
    (cond ((or (memq 'ENUM attr) (memq 'ENUMR attr))
	   (setq str (concat "<" str ">")))
	  ((memq 'RV attr) 
	   (setq str (concat "[" str "]"))))
    ; プリエディットの文字列を追加する
    (insert-and-inherit str)
    (setq end (point))
    ;; overlayによって属性を設定する
    (setq ol (make-overlay start end))
    (overlay-put ol 'face (anthy-select-face-by-attr attr))
    (setq anthy-preedit-overlays
	  (cons ol anthy-preedit-overlays))
    str))

(defvar anthy-select-candidate-keybind
  '((0 . "a")
    (1 . "s")
    (2 . "d")
    (3 . "f")
    (4 . "g")
    (5 . "h")
    (6 . "j")
    (7 . "k")
    (8 . "l")
    (9 . ";")))

(defun anthy-check-context-id ()
  (if (null anthy-context-id)
      (progn
	(setq anthy-context-id anthy-last-context-id)
	(setq anthy-last-context-id
	      (+ anthy-last-context-id 1)))))

(defun anthy-get-candidate (idx)
  "agentから候補を一つ取得する"
  (anthy-send-recv-command 
   (concat " GET_CANDIDATE "
	   (number-to-string idx) "\n")))

;; 候補リストからミニバッファに表示する文字列を構成する
(defun anthy-make-candidate-minibuffer-string (idx)
  (let ((cand-list anthy-enum-candidate-list)
	(cur-elm)
	(str))
    (while cand-list
      (setq cur-elm (car cand-list))
      (let ((cand-str (cdr (cdr cur-elm)))
	    (cand-idx (car (cdr cur-elm)))
	    (sel-idx (car cur-elm)))
	(setq str (format (if (= idx cand-idx)
			      "%s:[%s] "
			      "%s: %s  ")
			  (cdr (assoc sel-idx anthy-select-candidate-keybind))
			  cand-str)))
      (setq anthy-candidate-minibuffer
	    (concat str
		    anthy-candidate-minibuffer))
      (setq cand-list (cdr cand-list)))))

;; 表示する候補のリストに画面内でのインデックスを付ける
(defun anthy-add-candidate-index (lst)
  (let ((i 0)
	(res nil))
    (while lst
      (setq res
	    (cons
	     (cons i (car lst))
	     res))
      (setq i (1+ i))
      (setq lst (cdr lst)))
    res))


;; 表示する候補のリストを作る
(defun anthy-calc-candidate-layout (base nr l2r)
  (let ((width (frame-width))
	(errorp nil)
	(i 0)
	(repl)
	(cand-idx)
	(lst))
    (while (and
	    (if l2r
		(< (+ base i) nr)
	      (<= 0 (- base i)))
	    (> width 0)
	    (< i (length anthy-select-candidate-keybind))
	    (not errorp))
      (if l2r
	  (setq cand-idx (+ base i))
	(setq cand-idx (- base i)))
      (setq repl (anthy-get-candidate cand-idx))
      (if (listp repl)
	  ;; append candidate
	  (let ((cand-str (car repl)))
	    (setq width (- width (string-width cand-str) 5))
	    (if (> width 0)
		(setq lst
		      (cons
		       (cons cand-idx cand-str)
		       lst))))
	;; erroneous candidate
	(setq errorp t))
      (setq i (1+ i)))
    (if l2r
	(progn
	  (anthy-get-candidate (car (car lst)))
	  (setq lst (reverse lst))
	  ))
    (setq anthy-enum-candidate-list
	  (if (not errorp)
	      (anthy-add-candidate-index lst)
	    nil))))

;;
(defun anthy-layout-candidate (idx nr)
  "候補リストをminibufferへレイアウトする"
  (setq anthy-candidate-minibuffer "")
  (setq anthy-enum-candidate-list '())
  ;; 左->右 or 右->左にレイアウトする
  (if anthy-enum-candidate-p
      (anthy-calc-candidate-layout idx nr 't)
    (anthy-calc-candidate-layout idx nr nil))
  (anthy-make-candidate-minibuffer-string idx)
  ;; 結果を表示する
  (if anthy-enum-candidate-list
      (message "%s" anthy-candidate-minibuffer)
    nil))

(defun anthy-update-preedit (stat ps)
  "プリエディットを更新する"
  (let ((cursor-pos nil)
	(num-candidate 0)
	(idx-candidate 0))
    ;; erase old preedit
    (anthy-erase-preedit)
    (anthy-disable-preedit-keymap)
    ;; insert new preedit
    (setq anthy-preedit-start (point))
    (setq anthy-enum-candidate-p nil)
    (setq anthy-enum-rcandidate-p nil)
    (if (member stat '(2 3 4))
	(progn
	  (setq anthy-preedit
		(concat anthy-preedit "|"))
	  (anthy-insert-preedit-segment "|" '())))
    (while ps
      (let ((cur (car ps)))
	(setq ps (cdr ps))
	(cond
	 ((eq cur 'cursor)
	  (setq cursor-pos (point)))
	 ((string-equal (car (cdr cur)) "")
	  nil)
	 (t
	  (let ((nr (car (cdr (cdr (cdr cur)))))
		(idx (car (cdr (cdr cur))))
		(str (car (cdr cur)))
		(attr (car cur)))
	    (setq str (anthy-insert-preedit-segment str attr))
	    (cond ((and (car anthy-enable-enum-candidate-p) (memq 'ENUM attr))
		   (setq anthy-enum-candidate-p t)
		   (setq idx-candidate idx)
		   (setq num-candidate nr))
		  ((and (car anthy-enable-enum-candidate-p) (memq 'ENUMR attr))
		   (setq anthy-enum-rcandidate-p t)
		   (setq idx-candidate idx)
		   (setq num-candidate nr)))
	    (setq anthy-preedit
		  (concat anthy-preedit str))
	    (if (and (member stat '(3 4)) (not (eq ps '())))
		(progn
		  (setq anthy-preedit
			(concat anthy-preedit "|"))
		  (anthy-insert-preedit-segment "|" '()))))))))
    ;; enum candidate
    (if (or anthy-enum-candidate-p anthy-enum-rcandidate-p)
	(anthy-layout-candidate idx-candidate num-candidate))
    ;; update preedit keymap
    (if (member stat '(2 3 4))
	(anthy-enable-preedit-keymap))
    (if cursor-pos (goto-char cursor-pos))))

(if anthy-xemacs
    (defun anthy-encode-key (ch)
      (let* ((ccode (if (characterp ch)
			(char-to-int ch) nil))
	     (c))
	(setq c (assoc ccode anthy-keyencode-alist))
	(if c
	    (cdr c)
	  (setq c (assoc c anthy-keyencode-alist))
	  (if c
	      (cdr c)
	    (if (characterp ch)
		(char-to-string ch)
	      nil)))))
  (defun anthy-encode-key (ch)
    (let ((c (assoc ch anthy-keyencode-alist)))
      (if c
	  (cdr c)
	(if (and
	     (integerp ch)
	     (> ch 32))
	    (char-to-string ch)
	  nil)))))

(defun anthy-proc-agent-reply (repl)
  (let*
      ((stat (car repl))
       (body (cdr repl))
       (commit "")
       (preedit nil))
    ;; 各文節を処理する
    (while body
      (let* ((cur (car body))
	     (pe nil))
	(setq body (cdr body))
	(if (and
	     (listp cur)
	     (listp (car cur)))
	    (cond
	     ((eq (car (car cur)) 'COMMIT)
	      (setq commit (concat commit (car (cdr cur)))))
	     ((eq (car (car cur)) 'CUTBUF)
	      (let ((len (length (car (cdr cur)))))
		(copy-region-as-kill (point) (+ (point) len))))
	     ((memq 'UL (car cur))
	      (setq pe (list cur))))
	  (setq pe (list cur)))
	(if pe
	    (setq preedit (append preedit pe)))))
    ;; コミットされた文節を処理する
    (if (> (string-width commit) 0)
	(progn
	  (anthy-erase-preedit)
	  (anthy-disable-preedit-keymap)
	  (insert-and-inherit commit)))
    (anthy-update-preedit stat preedit)
    (anthy-update-mode-line)))

(defun anthy-insert-select-candidate (ch)
  (let* ((key-idx (car (rassoc (char-to-string ch)
			       anthy-select-candidate-keybind)))
	 (idx (car (cdr (assq key-idx
			      anthy-enum-candidate-list)))))
    (if idx
	(progn
	  (let ((repl (anthy-send-recv-command
		       (format " SELECT_CANDIDATE %d\n" idx))))
	    (anthy-proc-agent-reply repl))
	  (setq anthy-enum-candidate-p nil)
	  (setq anthy-enum-rcandidate-p nil))
      (message "%s" anthy-candidate-minibuffer))))

(defvar anthy-rkmap-keybind
  '(
    ;; q
    (("hiragana" . 113) . "katakana")
    (("katakana" . 113) . "hiragana")
    ;; l
    (("hiragana" . 108) . "alphabet")
    (("katakana" . 108) . "alphabet")
    ;; L
    (("hiragana" . 76) . "walphabet")
    (("katakana" . 76) . "walphabet")
    ;; \C-j
    (("alphabet" . 10) . "hiragana")
    (("walphabet" . 10) . "hiragana")))

;; xemacs はキーのハンドラ内で一部のキーに対して
;; その原因となったキーの情報を取得できない(?)ので
;; それらのキーごとにハンドラを書く
(defun anthy-insert-backspace (&optional arg)
  (interactive "*p") (anthy-handle-key 8 "(ctrl H)"))
(defun anthy-insert-left (&optional arg)
  (interactive "*p") (anthy-handle-key 2 "(left)"))
(defun anthy-insert-right (&optional arg)
  (interactive "*p") (anthy-handle-key 6 "(right)"))
;;
(defun anthy-insert (&optional arg)
  "Anthyのキーハンドラ"
  (interactive "*p")
  (let* ((ch last-command-char)
	 (chenc (anthy-encode-key ch)))
    (anthy-handle-key ch chenc)))

(defun anthy-handle-key (ch chenc)
  (cond
   ;; direct candidate selection
   ((and (or anthy-enum-candidate-p anthy-enum-rcandidate-p)
	 (integerp ch)
	 (assq (car (rassoc (char-to-string ch)
			    anthy-select-candidate-keybind))
	       anthy-enum-candidate-list))
    (anthy-insert-select-candidate ch))
   ;; map selection
   ((and (assoc (cons anthy-current-rkmap ch) anthy-rkmap-keybind)
	 (string-equal anthy-preedit ""))
    (let ((mapname (cdr (assoc (cons anthy-current-rkmap ch)
			       anthy-rkmap-keybind))))
      (let ((repl (anthy-send-recv-command 
		   (concat " MAP_SELECT " mapname "\n"))))
	(if (eq repl 'OK)
	    (progn
	      (setq anthy-current-rkmap
		    (cdr (assoc (cons anthy-current-rkmap ch)
				anthy-rkmap-keybind)))
	      (anthy-update-mode-line))))))
   ;; alphabet
   ((and (string-equal anthy-current-rkmap "alphabet")
	 (string-equal anthy-preedit ""))
    (self-insert-command 1))
   ;; normal input
   ((and
     (string-equal anthy-preedit "")
     (= ch 32)
     (not
      (string-equal anthy-current-rkmap "alphabet")))
    (insert-and-inherit anthy-wide-space))
   (t
    (let* ((repl
	    (if chenc (anthy-send-recv-command 
		       (concat chenc "\n"))
	      nil)))
      (if repl
	  (anthy-proc-agent-reply repl))))))

;;
(defun anthy-do-invoke-agent (cmd)
  (if (and (stringp anthy-personality)
	   (> (length anthy-personality) 0))
      (start-process "anthy-agent"
		     anthy-working-buffer
		     cmd
		     (concat " --personality=" anthy-personality))
    (start-process "anthy-agent"
		   anthy-working-buffer
		   cmd)))
;;
(defun anthy-invoke-agent ()
  (let ((list anthy-agent-command-list)
	(proc nil))
    (while (and list (not proc))
      (setq proc 
	    (anthy-do-invoke-agent (car list)))
      (if (not (boundp 'proc))
	  (setq proc nil))
      (setq list (cdr list)))
    proc))
;;
;;
;;
(defun anthy-check-agent ()
  ;; check and do invoke
  (if (not anthy-agent-process)
      (let
	  ((proc (anthy-invoke-agent)))
	(if anthy-agent-process
	    (kill-process anthy-agent-process))
	(setq anthy-agent-process proc)
	(process-kill-without-query proc)
	(cond ((coding-system-p 'euc-japan)
	       (set-process-coding-system proc 'euc-japan 'euc-japan))
	      ((coding-system-p '*euc-japan*)
	       (set-process-coding-system proc '*euc-japan* '*euc-japan*)))
	(set-process-sentinel proc 'anthy-process-sentinel))))
;;
(defun anthy-do-send-recv-command (cmd)
  (if (not anthy-agent-process)
      (anthy-check-agent))
  (let ((old-buffer (current-buffer)))
    (unwind-protect
	(progn
	  (set-buffer anthy-working-buffer)
	  (erase-buffer)
	  (process-send-string anthy-agent-process cmd)
	  (while (= (buffer-size) 0)
	    (accept-process-output nil 0 50))
	  (read (buffer-string)))
      (set-buffer old-buffer))))
;;
(defun anthy-send-recv-command (cmd)
  (if anthy-context-id
      (anthy-do-send-recv-command
       (concat " SELECT_CONTEXT "
	       (number-to-string anthy-context-id)
	       "\n")))
  (anthy-do-send-recv-command cmd))
;;
(defun anthy-minibuffer-enter ()
  (setq anthy-saved-mode anthy-mode)
  (setq anthy-mode nil)
  (setq anthy-enable-enum-candidate-p 
	(cons nil anthy-enable-enum-candidate-p))
  (anthy-update-mode))
;;
(defun anthy-minibuffer-exit ()
  (setq anthy-mode anthy-saved-mode)
  (setq anthy-enable-enum-candidate-p 
	(cdr anthy-enable-enum-candidate-p))
  (anthy-update-mode))
;;
(defun anthy-kill-buffer ()
  (anthy-send-recv-command
   " RELEASE_CONTEXT\n"))
;;
(defun anthy-mode-on ()
  (add-hook 'minibuffer-setup-hook 'anthy-minibuffer-enter)
  (add-hook 'minibuffer-exit-hook 'anthy-minibuffer-exit)
  (add-hook 'kill-buffer-hook 'anthy-kill-buffer)
  (anthy-check-context-id)
  (setq anthy-minor-mode t)
  (anthy-update-mode-line))
;;
(defun anthy-mode-off ()
  (setq anthy-minor-mode nil)
  (anthy-update-mode-line))
;;
(defun anthy-update-mode ()
  (if (or anthy-mode anthy-leim-active-p)
      (progn
	(anthy-check-agent)
	(anthy-mode-on))
    (anthy-mode-off)))

(defun anthy-mode (&optional arg)
  "Start Anthy conversion system."
  (interactive "P")
  (setq anthy-mode
        (if (null arg)
            (not anthy-mode)
          (> (prefix-numeric-value arg) 0)))
  (anthy-update-mode))
;;
(defun anthy-hiragana-map (&optional arg)
  "Hiragana mode"
  (interactive "P")
  (anthy-send-recv-command " MAP_SELECT hiragana\n"))
;;
(defun anthy-katakana-map (&optional arg)
  "Hiragana mode"
  (interactive "P")
  (anthy-send-recv-command " MAP_SELECT katakana\n"))
;;
(defun anthy-alpha-map (arg)
  "Alphabet mode"
  (interactive "P")
  (anthy-send-recv-command " MAP_SELECT alphabet\n"))
;;
(defun anthy-wide-alpha-map (arg)
  "Wide Alphabet mode"
  (interactive "P")
  (anthy-send-recv-command " MAP_SELECT walphabet\n"))
;;
;;
;; leim の inactivate
;;
(defun anthy-leim-inactivate ()
  (setq anthy-leim-active-p nil)
  (anthy-update-mode))
;;
;; leim の activate
;;
(defun anthy-leim-activate (&optional name)
  (setq inactivate-current-input-method-function 'anthy-leim-inactivate)
  (setq anthy-leim-active-p t)
  (anthy-update-mode)
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'anthy-leim-exit-from-minibuffer)))

;;
;; emacsのバグ避けらしいです
;;
(defun anthy-leim-exit-from-minibuffer ()
  (inactivate-input-method)
  (when (<= (minibuffer-depth) 1)
    (remove-hook 'minibuffer-exit-hook 'anthy-leim-exit-from-minibuffer)))
;;
;;
;;
(global-set-key [(meta escape)] 'anthy-mode)
(provide 'anthy)

(load "anthy-dic")
(load "anthy-conf")

;; these should go away for 18n
(if (boundp 'default-input-method)
    (setq-default default-input-method "japanese-anthy"))
(setq default-input-method "japanese-anthy")
;;;
;;; anthy.el ends here
