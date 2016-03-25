;;; Code:
;; Syntaxハイライト設定
(defface font-lock-c-di-face '((t (:foreground "blue" :bold t))) nil)
(defface font-lock-c-ei-face '((t (:foreground "red" :bold t))) nil)
(font-lock-add-keywords
 'c-mode '(
					 ("\\<m?i_[a-zA-Z0-9_]+\\>" . 'font-lock-variable-name-face)
					 ("\\<IntcrlDisableAllInterrupt\\>()" . 'font-lock-c-di-face)
					 ("\\<IntcrlEnableAllInterrupt\\>()" . 'font-lock-c-ei-face)
					 ("\\<MYSTATIC\\>" . 'font-lock-keyword-face)
					 ("\\<\\(ST\\|EM\\)_[a-zA-Z0-9_]+\\>" . 'font-lock-type-face)
					 ("\\<[A-Z][A-Z0-9_]+[A-Z0-9]\\>" . 'font-lock-constant-face)
					 ("\\<\\(VALID\\|INVALID\\)\\>" . 'font-lock-constant-face)
					 ))

(when (require 'color-moccur nil t)
	(add-hook 'c-mode-common-hook '(lambda () (setq moccur-grep-default-mask "\\.[ch]$")))
	)

(defun c-func-list ()
	(interactive)
	(occur-by-moccur "^[_a-zA-Z][^()+-=;:]*\\s-+[_a-zA-Z][_a-zA-Z0-9]*(.*$" " ")
	)

;;コメントスタイル変更
(add-hook 'c-mode-common-hook '(lambda () (setq comment-style 'box-multi)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 111-c-mode.el ends here
