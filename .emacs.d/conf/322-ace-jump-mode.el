;;; Code:
(require 'ace-jump-mode)
(setq ace-jump-mode-gray-background nil)
(setq ace-jump-word-mode-use-query-char nil)
(setq ace-jump-mode-move-keys
			(append "asdfghjkl;:]qwertyuiop@zxcvbnm,." nil))
(define-key global-map (kbd "C-:") 'ace-jump-word-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 322-ace-jump-mode.el ends here
