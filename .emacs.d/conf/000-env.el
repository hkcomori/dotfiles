;;-*- coding: utf-8; -*-
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境変数                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar oldemacs-p (<= emacs-major-version 22)) ;22 以下
(defvar emacs23-p (<= emacs-major-version 23))	;23 以下
(defvar emacs24-p (>= emacs-major-version 24))	;24 以上

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パス設定                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
	(let (path)
		(dolist (path paths paths)
			(let ((default-directory
							(expand-file-name (concat user-emacs-directory path))))
				(add-to-list 'load-path default-directory)
				(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
						(normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path
 "elpa"																;ELPAでインストールしたElisp
 "elisp"															;自作のElisp
 "site-lisp"													;gnupack標準 & ダウンロードしたElisp
 )

;; Local Variables:
;; coding: utf-8
;; End:

;;; 000-env.el ends here
