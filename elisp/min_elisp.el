(menu-bar-mode -1)
(tool-bar-mode -1)

(global-set-key [f5] (lambda () (interactive) (progn (shell "*f5-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f6] (lambda () (interactive) (progn (shell "*f6-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f7] (lambda () (interactive) (progn (shell "*f7-shell*") (set-buffer-process-coding-system 'utf-8 'utf-8))))
(global-set-key [f8] (lambda () (interactive) (progn (shell) (set-buffer-process-coding-system 'utf-8 'utf-8))))

(defun my-pushd-current-directory (&optional buffer)
  (interactive)
  (let ((target-dir (expand-file-name ".")))
    (if (> (count-windows) 1)
	(other-window-except-minibuffer-window) 
      (split-window-vertically))
    (my-input-command-to-shell (concat "pushd " target-dir) buffer)))

(global-set-key [(control f5)] '(lambda () (interactive) (my-pushd-current-directory "*f5-shell*")))
(global-set-key [(control f6)] '(lambda () (interactive) (my-pushd-current-directory "*f6-shell*")))
(global-set-key [(control f7)] '(lambda () (interactive) (my-pushd-current-directory "*f7-shell*")))

(global-set-key [(control f8)] 'my-pushd-current-directory)
(global-set-key "\C-t" 'copy-region-as-kill)
