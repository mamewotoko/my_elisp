;; myposition_history.el	Created      : Tue Jan  6 01:01:46 2004
;;			Last modified: Sat Jul 07 21:00:41 2018
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;;

(defconst position-buffer-name "position-buffer")
(global-set-key "\C-v" 
  '(lambda () (interactive)
     (save-excursion
       (let ((target-buffer (get-buffer position-buffer-name))
	     (old-buffer (current-buffer)))
	 (if (and target-buffer (not (eq target-buffer (current-buffer))))
	   (let ((filename (buffer-file-name)))
	     (if filename 
		 (let ((linenum (line-number)))
		   (progn
		     (set-buffer target-buffer)
		     (goto-char (point-max))
		     (insert (format "%s:%d:\n" filename linenum)))))))))
       (other-window 1)))


(provide 'myposition_history)
