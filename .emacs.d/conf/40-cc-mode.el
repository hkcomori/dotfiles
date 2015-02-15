;;(defmacro lazyload (func lib docstring &rest body)
;;  "遅延ロード．funcにオートロードさせたい関数を並べる．
;;例：\(lazyload \(func1 func2\) \"hogehoge\"\)"
;;  (declare (indent 3))
;;  `(when (locate-library ,lib)
;;     ,@(mapcar (lambda (f) `(autoload ',f ,lib ,docstring t)) func)
;;     (eval-after-load ,lib
;;       `(funcall #',(lambda () ,@body)))))
;;(defmacro append-to-list (to list)
;;  " list に append する際に要素を複数指定"
;;  (declare (indent 1))
;;  `(setq ,to (append ,list ,to)))
;;
;;(when (require 'align nil t)
;;	(add-to-list 'align-rules-list
;;							 '(cc-comments-frame0
;;								 (regexp . "\\/\\*\\(\\s-+\\)\\*\\/")
;;								 (modes  . '(cc-mode))))
;;)
;;
;;(when (require 'align nil t)
;;	(lazyload (align align-regexp align-newline-and-indent) "align" nil
;;		(append-to-list align-rules-list
;;			(list '(cc-comment-frame
;;							(regexp . "\\(\\s-*\\):")
;;							(modes . '(cc-mode)))
;;						'(cc-comment-frame2
;;							(regexp . "\\(\\s-+\\)\\*\\/")
;;							(modes . '(cc-mode)))
;;;;							'(cc-comment-frame3
;;						)))
;;)
