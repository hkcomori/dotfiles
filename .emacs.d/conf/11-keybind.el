(global-set-key (kbd "C-x :") 'goto-line)				;指定行に移動する
;(global-set-key (kbd "C-x C-b") 'bs-show)				;bs-showでバッファ選択する。
(global-set-key (kbd "C-z") nil)						;何もしない
(global-set-key (kbd "C-h") 'delete-backward-char)		;前の文字を消す
(global-set-key (kbd "M-d") 'delete-word)				;次の単語を消す
(global-set-key (kbd "M-h") 'backward-delete-word)		;前の単語を消す
(global-set-key (kbd "M-C-g") 'grep-find)				;カレントディレクトリ以下をGrep検索
(global-set-key (kbd "C-c a") 'align)					;アラインメント
(global-set-key [f3] 'isearch-forward)
(global-set-key [S-f3] 'isearch-backward)

(global-set-key (kbd "C-m") 'newline-and-indent)		;改行と同時にインデント
(add-hook 'c-mode-common-hook		'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'sh-mode-hook					'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'python-mode-hook			'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'ruby-mode-hook				'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))

(global-set-key (kbd "C-\\") 'toggle-input-method)		;IMEをオン/オフ
(global-set-key [zenkaku-hankaku] 'toggle-input-method)	;IMEをオン/オフ
(global-set-key [M-kanji] 'ignore)						;何もしない
(global-set-key [kanji] 'ignore)						;何もしない
(global-set-key (kbd "M-N") 'move-line-down)			;行を一行下へ
(global-set-key (kbd "M-P") 'move-line-up)				;行を一行上へ

(global-set-key [C-down-mouse-1] 'mouse/mark-current-sexp)
(global-set-key [C-mouse-1] 'mouse/mark-current-sexp)
(global-set-key [C-double-mouse-1] 'mouse/mark-current-sexp)
