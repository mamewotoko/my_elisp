;; mysearchfile.el	Created      : Fri Dec 26 03:39:52 2003
;;			Last modified: Fri Dec 26 03:42:24 2003
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;;

;; TODO:
;; locate コマンドを用いたファイル検索
;; find ..
;; 複数候補の見せ方?

;; shellmode でファイルを検索する時にディレクトリスタックを
;; もとに検索する

(defun search-file-from-dirstack (filename)
  (locate-file filename shell-dirstack))

(provide 'mysearchfile)
