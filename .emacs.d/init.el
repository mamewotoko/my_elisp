(setq load-path
      (append (list (expand-file-name "~/lib/emacs/elisp/")
		    (expand-file-name "~/lib/emacs/elisp/ess/")
		    (expand-file-name "~/lib/emacs/elisp/ssh-el/")
            (expand-file-name "~/.emacs.d/elpa/csv-mode-1.12/")
		    (expand-file-name "~/.opam/system/share/emacs/site-lisp/"))
		    load-path))
(defvar browse-url-mosaic-program nil)
(load "myinit.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   '("--line-number" "--smart-case" "--nogroup" "--column" "--stats" "--width" "500" "-z" "--"))
 '(ag-executable "/usr/local/bin/ag")
 '(ag-highlight-search nil)
 '(ag-reuse-buffers t)
 '(ag-reuse-window nil)
 '(compilation-error-screen-columns nil)
 '(copyright-query nil)
 '(copyright-regexp
   "\\(©\\|(c)\\|@copyright{}\\|[Cc]opyright\\s *:?\\s *\\(?:(C)\\)?\\|[Cc]opyright\\s *:?\\s *©\\)\\s *\\(?:[^0-9
                                                                                                              ]*\\s *\\)?\\([1-9]\\([-0-9, ';/*%#
        ]\\|\\s<\\|\\s>\\)*[0-9]+\\)")
 '(dabbrev-case-fold-search nil)
 '(dired-recursive-deletes 'always)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-default-load-average nil)
 '(display-time-load-average-threshold 2)
 '(display-time-string-forms
   '((format "%s/%s/%s(%s)
             %s:%s" year month day dayname 24-hours minutes)))
 '(docker-tramp-use-names t)
 '(enable-local-variables :safe)
 '(enable-recursive-minibuffers t)
 '(epg-gpg-program "/usr/local/bin/gpg")
 '(fill-column 80)
 '(flycheck-flake8rc ".flake8")
 '(flycheck-pycheckers-checkers '(flake8))
 '(flycheck-python-flake8-executable "/usr/local/bin/flake8")
 '(flymake-python-pyflakes-extra-arguments
   '("--max-line-length=120" "--ignore=E128,D103,E501,D100,D103"))
 '(git-link-commit-remote-alist
   '(("git.sr.ht" git-link-commit-github)
     ("github" git-link-commit-github)
     ("bitbucket" git-link-commit-bitbucket)
     ("gitorious" git-link-commit-gitorious)
     ("gitlab" git-link-commit-github)
     ("mamewo" git-link-kallithea)))
 '(helm-dabbrev-case-fold-search t)
 '(helm-dabbrev-cycle-threshold 1)
 '(helm-mm-matching-method 'multi2)
 '(helm-mode-fuzzy-match t)
 '(help-at-pt-timer-delay 0.9)
 '(imagemagick-enabled-types
   '(3FR ART ARW AVS BMP BMP2 BMP3 CAL CALS CMYK CMYKA CR2 CRW CUR CUT DCM DCR DCX DDS DJVU DNG DPX EXR FAX FITS GBR GIF GIF87 GRB HRZ ICB ICO ICON J2C JNG JP2 JPC JPEG JPG JPX K25 KDC MIFF MNG MRW MSL MSVG MTV NEF ORF OTB PBM PCD PCDS PCL PCT PCX PDB PEF PGM PICT PIX PJPEG PNG PNG24 PNG32 PNG8 PNM PPM PSD PTIF PWP RAF RAS RBG RGB RGBA RGBO RLA RLE SCR SCT SFW SGI SR2 SRF SUN SVG SVGZ TGA TIFF TIFF64 TILE TIM TTF UYVY VDA VICAR VID VIFF VST WBMP WPG X3F XBM XC XCF XPM XV XWD YCbCr YCbCrA YUV jpg))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(initial-buffer-choice t)
 '(line-move-visual nil)
 '(lsp-ocaml-lang-server-command '("/usr/local/bin/ocaml-language-server" "--stdio"))
 '(magit-commit-arguments nil)
 '(magit-completing-read-function 'helm--completing-read-default)
 '(mode-line-format
   '("%e" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification " " mode-line-position
     (vc-mode vc-mode)
     " " mode-line-modes mode-line-misc-info))
 '(mode-line-percent-position nil)
 '(monky-outgoing-repository "default")
 '(next-line-add-newlines nil)
 '(ns-command-modifier 'meta)
 '(org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.10/libexec/ditaa0_10.jar")
 '(org-mode-hook
   '(#[0 "\300\301\302\303\304$\207"
         [add-hook change-major-mode-hook org-show-block-all append local]
         5]
     #[0 "\300\301\302\303\304$\207"
         [add-hook change-major-mode-hook org-babel-show-result-all append local]
         5]
     org-babel-result-hide-spec org-babel-hide-all-hashes))
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(haskell-mode greader package-utils lsp-treemacs posframe lsp-scala scala-mode helm-ag-r zap-to-char csv-mode helm-gtags elpy company-lsp zop-to-char git-auto-commit-mode ac-skk ddskk-posframe csv web-mode company-terraform auto-complete lsp-ui lsp-ocaml lsp-mode doom-themes direx helm-descbinds dockerfile-mode graphviz-dot-mode weather websocket jedi helm-emms emms tramp-theme helm speed-type gtags doom-modeline neotree use-package spaceline-all-the-icons all-the-icons ggtags ssh-config-mode flycheck-pycheckers w3m git-link japanlaw japanese-holidays smex ido-ubiquitous ido-vertical-mode ducpel sokoban ddskk tuareg gitter slack circe twittering-mode yaml-mode ## etags-select etags-table terraform-mode flycheck-ocaml merlin flycheck package-build shut-up epl git commander f dash s docker-tramp go-mode ssh svg php-mode monky wgrep-ag popup nhexl-mode markdown-mode magit ht exec-path-from-shell ag csv-mode))
 '(python-check-command "/usr/local/bin/flake8")
 '(python-flymake-command '("/usr/local/bin/flake8"))
 '(require-final-newline t)
 '(show-trailing-whitespace t)
 '(ssh-directory-tracking-mode t)
 '(tab-width 4)
 '(tetris-buffer-height 52)
 '(tetris-height 50)
 '(tuareg-default-indent 2)
 '(tuareg-function-indent 0)
 '(tuareg-in-indent 0)
 '(tuareg-let-always-indent t)
 '(tuareg-with-indent 0)
 '(warning-suppress-types '((\(undo\ discard-info\) ##)))
 '(web-mode-markup-indent-offset 2)
 '(whitespace-style
   '(empty indentation trailing tabs space-after-tab space-before-tab tab-mark)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((default nil)))
 '(markdown-code-face ((t (:inherit fixed-pitch :background "#333333"))))
 '(trailing-whitespace ((t nil)))
 '(yaml-tab-face ((t nil))))
(put 'narrow-to-region 'disabled nil)

(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
