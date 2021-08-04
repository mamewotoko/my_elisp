;;;;;;;;;;;;;
;;; mycolor.el --- 
;; Copyright (C) 2000 by Free Software Foundation, Inc.

;; Author: MASUYAMA Takashi <tak@ise45.is.s.u-tokyo.ac.jp>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

;;; text properties
;;; コメント、予約語などの色の設定
(require 'font-lock)
(defun my-font-lock-set-face ()
  ;;
  ;; 'if' や 'while' といった予約語
  (make-face 'my-keyword-face)
  (set-face-foreground 'my-keyword-face "BlueViolet")
  (setq font-lock-keyword-face 'my-keyword-face)
  ;(setq font-lock-keyword-face 'bold)
  ;;
  ;; コメント
  (make-face 'my-comment-face)
  (set-face-foreground 'my-comment-face "MediumBlue")
  (setq font-lock-comment-face 'my-comment-face)
  ;;
  ;; 文字列
  (make-face 'my-string-face)
  (set-face-foreground 'my-string-face "VioletRed")
  (setq font-lock-string-face 'my-string-face)
  ;;
  ;; 関数名
  (make-face 'my-function-face)
  (set-face-foreground 'my-function-face "red")
  (setq font-lock-function-name-face 'my-function-face)
  ;(setq font-lock-function-name-face 'default)
  ;;
  ;; 'typedef' や 'class'
  (make-face 'my-type-face)
  (set-face-foreground 'my-type-face "firebrick")
  (setq font-lock-type-face 'my-type-face)
  ;(setq font-lock-type-face 'default)

  ;;変数
  (make-face 'my-variable-face)
  (set-face-foreground 'my-variable-face "DarkGreen")
  (setq font-lock-variable-name-face 'my-variable-face)
  ;;
  ;; document
  ;;(make-face 'my-doc-string-face)
  ;;(set-face-foreground 'my-doc-string-face "lightgrey")
  ;;(setq font-lock-doc-string-face 'my-doc-string-face)
)

(add-hook 'font-lock-mode-hook
	  '(lambda ()
	     (my-font-lock-set-face)))

(provide 'mycolor)
;;; mycolor.el ends here
;;;;;;;;;;;;;;;;;;;;;;;