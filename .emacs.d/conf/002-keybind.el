;;; Code:

;; カーソル移動
(define-key global-map (kbd "C-x :")						'goto-line)							;指定行に移動する
(define-key global-map (kbd "C-M-<right>")			'forward-sexp)					;次のS式
(define-key global-map (kbd "C-M-<left>")				'backward-sexp)					;前のS式

;; 編集
(define-key global-map (kbd "C-c a")						'align)									;アラインメント
(define-key global-map (kbd "C-h")							'delete-backward-char)	;前の文字を消す
(define-key global-map (kbd "M-d")							'delete-word)						;次の単語を消す
(define-key global-map (kbd "M-h")							'backward-delete-word)	;前の単語を消す

;; 改行
(define-key global-map (kbd "C-m")							'newline-and-indent)		;改行と同時にインデント
(add-hook 'c-mode-common-hook		'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'sh-mode-hook					'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'python-mode-hook			'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))
(add-hook 'ruby-mode-hook				'(lambda () (local-set-key (kbd "C-m") 'reindent-then-newline-and-indent)))

;; 使わないキーバインドの無効化
(define-key global-map [zenkaku-hankaku]				'toggle-input-method)		;IMEをオン/オフ
(define-key global-map [M-kanji]								'ignore)								;何もしない
(define-key global-map [kanji]									'ignore)								;何もしない
(define-key global-map (kbd "C-z")							'ignore)								;何もしない

;; サクラエディタとの互換キーバインド
(define-key global-map [f3] 'isearch-forward)
(define-key global-map [S-f3] 'isearch-backward)

;; マウス
(define-key minibuffer-inactive-mode-map [mouse-1] 'ignore)
(define-key global-map [C-down-mouse-1] 'mouse/mark-current-sexp)
(define-key global-map [C-mouse-1] 'mouse/mark-current-sexp)
(define-key global-map [C-double-mouse-1] 'mouse/mark-current-sexp)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 002-keybind.el ends here
