;;; myjavamode.el --- Last modified: Thu Nov 19 19:36:05 2015
;; Author: Takashi Masuyama <mamewo@dk9.so-net.ne.jp>
;; FTP Directory: sources/emacs #
;; Location: http://www002.upp.so-net.ne.jp/mamewo/sources/emacs/myjavamode.el #

(defvar my-java-api-base "/mnt/other/tak/Doc/docs/api")
(defvar my-java-api-allclasses (concat my-java-api-base "allclasses-frame.html"))
(defvar my-java-api-search-output-buffer "*Shell Command Output*")
(defconst java-namazu-dir "/home/tak/.indexes/java")

(defun my-java-search-api (class &optional kill apropos)
  ;(interactive)
  (let ((search-obj (if apropos class (format "/%s.html" class))))
    ;; 注意 shell-command はステータスコードを返すのと、t,nil
    ;;を返すのがあるようです(バージョン依存)ステータスコードを
    ;;返される場合は 返り値が0であるかを見るように変更して下さい。
    ;; xemacs --> t/nil
    ;; emacs  --> status か??
    ;; (= (shell-command (format "grep %s %s search-obj
    ;;                            my-java-api-search-output-buffer))
    ;;     0) と変更する
    (if (shell-command (format "grep %s %s" search-obj my-java-api-allclasses))
	(progn
	  (if (not kill)
	      (if (> (count-windows) 1)
		  (other-window 1) ;;minibufferに入ると....
		(split-window-vertically)))
	  (switch-to-buffer my-java-api-search-output-buffer)
	  (beginning-of-buffer)
	  (search-forward "\"")
	  (let ((start (point)))
	    (search-forward "\"")
	    (let* ((end (- (point) 1))
		   (path (buffer-substring start end)))
	      (if kill (kill-buffer my-java-api-search-output-buffer))
	      (setq path (concat "file:" my-java-api-base path))
	      (browse-url path))))
      (message "Not found"))))


(defvar my-search-directory-comet
  (list "/home/tak/project/comet-1.0/comet/src/comet/mobile/core"
	"/home/tak/project/comet-1.0/comet/src/comet/mobile/mobile"))

;;(defvar my-search-directories my-search-directory-emfg)
  
(defun my-java-open-source-file (class path-list)
  (let* ((filename (concat class ".java"))
	 (path (locate-library filename nil path-list)))
    (if path
	(find-file-other-window path)
      (message (concat "Not found the file: " filename)))))

;; 個人的に読んでいるjavaプログラムです↓
(defun my-java-search-source-file-of-comet ()
  (interactive)
  (let ((class (my-word-at-position)))
    (my-java-open-source-file class my-search-directory-comet)))

(defun my-java-extend-menu (&optional index)
  (interactive "P")
  (message "O)utput F)or I)f L)ink T)ry")
  (let ((sw (selected-window))
	(local-index (or index (read-char))))
    (message nil)
    (cond
     ((= local-index ?o) (progn (insert-string "System.out.println();")
				(backward-char 2)))
     ((= local-index ?f) (c++-insert-incremental-for))
     ((= local-index ?i) (my-c-insert-if))
     ((= local-index ?l) (progn (insert-string "{@link #}") (backward-char 2)))
     ((= local-index ?t) (insert-string "try {\n} catch (Exception e) {\n    e.printStackTrace();\n}"))
     )))

(defun my-java-search-api-at-point ()
  (interactive)
  (let ((obj (my-word-at-position)))
    (if (null obj)
	(message "Irregural Position!")
      (my-java-search-api obj t))))

(defun my-java-apropos-search-api-at-point ()
  (interactive)
  (let ((obj (my-word-at-position))) ;; regionあった方が良い
    (if (null obj)
	(message "Irregural Position!")
      (my-java-search-api obj nil t))))


(defun java-template-function ()
  (message "a)pplet or s)wing or A)wt or not")
  (let* ((ch (read-char))
	 (appret-q (eq ch ?a))
	 (swing-q (eq ch ?s))
	 (awt-q   (eq ch ?A))
	 (file-name (file-name-nondirectory (buffer-file-name)))
	 (prefix (file-name-prefix file-name)))
    (insert-header "//" "")
    (insert-string 
     (format "//\t\t\t%s\n// %s javac %s %s\n// %s %s %s %s\n// FTP Directory: sources/java #\n//%s\n//\n"
	     time-stamp-start
	     mycompile-start

	     file-name ;; javac [filaneme] #
	     mycompile-end

	     myexecute-start
	     (if appret-q "appletviewer" "java")
	     (if appret-q file-name prefix)
	     mycompile-end
     my-line))
    (insert-string
     (concat "import java.io.*;\n"
	     "import java.util.*;\n"
	     (cond 
	      (appret-q (concat "import java.applet.Applet;\n"
				"import java.awt.*;\n"
				"import java.awt.event.*;\n"
				"/*\n  <applet code=" prefix " width=400 height=400>"
				 "</applet>\n */\n\n\n"))
	      (swing-q (concat "import javax.swing.*;\nimport java.awt.event.*;\nimport java.awt.*;\n\n"))
	      (t ""))

	     "public class " prefix 
	     (if appret-q
		 " extends Applet")
	     " {\n"
	     (if appret-q
		 (concat 
		  "    public void init() {\n\n    }\n"
		  "    public void start() {\n\n    }\n"
		  "    public void paint(Graphics g) {\n\n    }\n}"))
	     (if swing-q
		 (concat 
		  "    public static void main(String argv[])\n"
		  "    {\n"
		  "        JFrame.setDefaultLookAndFeelDecorated(true);\n"
		  "        JFrame f = new JFrame();\n"
		  "        f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);\n"
		  "        Container cont = f.getContentPane();\n\n\n"
		  "        f.pack();\n        f.setVisible(true);\n"
		  "    }\n"
		  "}\n"))
	     (if (and (not appret-q) (not swing-q))
		 (concat "    public static void main(String argv[])\n    {\n        try {\n\n        } catch (Exception e) {\n            e.printStackTrace();\n        }\n    }\n}"))))
    (previous-line (if appret-q 12 6))))

(defvar my-java-search-path '("."))

;  (and (string-match "(^\tat|\\[javac\\]\\) [^\(]+(\\([^:]+\\):\\([0-9]+\\))" error-line)

(defun my-java-jump-at-exception (error-message &optional path)
  ;;(and (string-match "javac. [^\(]+(\\([^:]+\\):\\([0-9]+\\))" error-message)
  (and (string-match "\\[javac\\] \\([^ :][^:]+\\):\\([0-9]+\\):"error-message)  ;; lint warning
       (let* ((filename (match-string 1 error-message))
	      (searchpath (or path my-java-search-path))
	      (linenumber (match-string 2 error-message))
	      (filepath (locate-file filename searchpath)))
	 (my-error-jump-to-point filename linenumber))))

(provide 'myjavamode)

;;; myjavamode.el ends here
