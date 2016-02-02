(load-file "~/.emacs.d/conf/000-env.el")
(load-file "~/.emacs.d/conf/001-aliases.el")
(load-file "~/.emacs.d/conf/002-keybind.el")

(defun delete-word (arg)
	(interactive "p")
	(delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
	(interactive "p")
	(delete-word (- arg)))
