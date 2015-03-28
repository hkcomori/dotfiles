(define-key global-map (kbd "C-x C-c") 'my/close-all-unmodified-buffer)					;バッファをすべて閉じる
(define-key global-map (kbd "C-x :") 'goto-line)																;指定行に移動する
(define-key global-map (kbd "C-z") nil)																					;何もしない
(define-key global-map (kbd "C-h") 'delete-backward-char)												;前の文字を消す
(define-key global-map (kbd "M-d") 'delete-word)																;次の単語を消す
(define-key global-map (kbd "M-h") 'backward-delete-word)												;前の単語を消す
(define-key global-map (kbd "M-g") 'grep-find)																	;カレントディレクトリ以下をGrep検索
(define-key global-map (kbd "C-c a") 'align)																		;アラインメント
(define-key global-map [f3] 'isearch-forward)
(define-key global-map [S-f3] 'isearch-backward)

(define-key global-map (kbd "C-m") 'newline-and-indent)		;改行と同時にインデント
(add-hook 'c-mode-common-hook		'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'sh-mode-hook					'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'python-mode-hook			'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'ruby-mode-hook				'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))

(define-key global-map [zenkaku-hankaku] 'toggle-input-method)	;IMEをオン/オフ
(define-key global-map [M-kanji] 'ignore)											;何もしない
(define-key global-map [kanji] 'ignore)												;何もしない
(define-key global-map (kbd "M-N") 'move-line-down)						;行を一行下へ
(define-key global-map (kbd "M-P") 'move-line-up)							;行を一行上へ

(define-key global-map [C-down-mouse-1] 'mouse/mark-current-sexp)
(define-key global-map [C-mouse-1] 'mouse/mark-current-sexp)
(define-key global-map [C-double-mouse-1] 'mouse/mark-current-sexp)
