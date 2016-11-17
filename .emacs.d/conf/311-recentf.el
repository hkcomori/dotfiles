;;; Code:
(require 'recentf)
(defvar my-recentf-list-prev nil)
(defadvice recentf-save-list
		(around no-message activate)
	"If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
	(unless (equal recentf-list my-recentf-list-prev)
		(cl-flet ((message (format-string &rest args)
											 (eval `(format ,format-string ,@args)))
							(write-file (file &optional confirm)
													(let ((str (buffer-string)))
														(with-temp-file file
															(insert str)))))
			ad-do-it
			(setq my-recentf-list-prev recentf-list))))
(defadvice recentf-cleanup
		(around no-message activate)
	"suppress the output from `message' to minibuffer"
	(cl-flet ((message (format-string &rest args)
										 (eval `(format ,format-string ,@args))))
		ad-do-it))

;;(setq recentf-exclude '("^\\.emacs\\.bmk$"))
(setq recentf-max-menu-items 20)
(setq recentf-max-saved-items 2000)
(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
(setq recentf-exclude '(".recentf"))
(setq recentf-auto-cleanup 60)
(setq recentf-auto-save-timer (run-with-idle-timer 900 t 'recentf-save-list))
(recentf-mode t)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 311-recentf.el ends here
