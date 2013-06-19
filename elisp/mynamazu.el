;; mynamazu.el		Created      : Sun Dec 14 11:47:58 2003
;;			Last modified: Sun Dec 14 11:51:01 2003
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;;

(defun set-namazu-dir-of-buffer (dir)
  (make-local-variable 'namazu-default-dir)
  (setq namazu-default-dir dir))

(defun set-namazu-dir-of-buffer-command ()
  (interactive)
  (read-input "namazu dir: " (or namazu-default-dir "")))

(provide 'mynamazu)
