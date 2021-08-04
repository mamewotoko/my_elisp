;; mylookup.el		Created      : Sat Dec  6 16:49:19 2003
;;			Last modified: Sat Dec 06 16:53:01 2003
;;------------------------------------------------------------
;; Written by Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs ;;

(autoload 'lookup "lookup" nil t)
(autoload 'lookup-region "lookup" nil t)
(autoload 'lookup-pattern "lookup" nil t)
(define-key ctl-x-map "l" 'lookup)              ; C-x l - lookup
(define-key ctl-x-map "y" 'lookup-region)       ; C-x y - lookup-region
(define-key ctl-x-map "\C-y" 'lookup-pattern)   ; C-x C-y - lookup
(setq lookup-search-agents '((ndtp "venus.is.s.u-tokyo.ac.jp")))

(add-hook 'lookup-entry-mode-hook
          '(lambda () 
             (define-key lookup-entry-mode-map
               "j" 'lookup-entry-next-entry)
             (define-key lookup-entry-mode-map
               "k" 'lookup-entry-previous-entry)))
