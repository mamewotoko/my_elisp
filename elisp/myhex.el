;; myhex.el		Created      : Sun Nov 16 12:25:56 2003
;;			Last modified: Sun Nov 16 12:30:21 2003
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs ;;

(defvar hex-table [ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F" ])
(defun char-to-hex-string (c)
  (let* ((code (char-to-int c))
	 (hd (aref hex-table (/ c 16)))
	 (tl (aref hex-table (% c 16))))
    (concat hd tl)))

