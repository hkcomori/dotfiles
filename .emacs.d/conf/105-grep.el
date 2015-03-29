;;; Code:
(require 'grep)
(when (eq system-type 'gnu/linux)
	(grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Au8 -Ia  {} +" . 40))
	(grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
	)
(when (eq system-type 'cygwin)
	(grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Asjis -Ia  {} +" . 42))
	(grep-apply-setting 'grep-command "lgrep -n -Asjis -Ia ")
	)

;; grep結果を編集可能にする
(when (require 'wgrep nil t)
	(define-key grep-mode-map "e" 'wgrep-change-to-wgrep-mode)
)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 105-grep.el ends here
