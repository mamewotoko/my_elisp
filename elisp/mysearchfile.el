;; mysearchfile.el	Created      : Fri Dec 26 03:39:52 2003
;;			Last modified: Fri Dec 26 03:42:24 2003
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;;

;; TODO:
;; locate ���ޥ�ɤ��Ѥ����ե����븡��
;; find ..
;; ʣ������θ�����?

;; shellmode �ǥե�����򸡺�������˥ǥ��쥯�ȥꥹ���å���
;; ��Ȥ˸�������

(defun search-file-from-dirstack (filename)
  (locate-file filename shell-dirstack))

(provide 'mysearchfile)
