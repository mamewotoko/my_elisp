;; mybrowseurl.el	Created      : Sat Dec 13 16:24:32 2003
;;			Last modified: Sun Dec 17 01:34:35 2006
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #

(defun browse-by-mozilla-firebird (url arg)
  (shell-command (format "/usr/local/firefox/firefox -remote 'openurl(%s,new-tab)'" url)))
(setq browse-url-browser-function 'browse-by-mozilla-firebird)

(provide 'mybrowseurl)

