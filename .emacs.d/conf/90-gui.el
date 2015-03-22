(when window-system
	(modify-frame-parameters nil '((wait-for-wm . nil)))	 ;;GUI起動を高速化
	(menu-bar-mode -1)							 ;;メニューバーを非表示
	(tool-bar-mode -1)							 ;;ツールバーを非表示
	(scroll-bar-mode -1)						 ;;スクロールバーを非表示
	(setq frame-title-format (list "%b - Emacs@" (system-name)))
	(set-face-background 'fringe (face-attribute 'default :background))
	(set-face-foreground 'font-lock-comment-face "gray")
	;; モードライン
	(set-face-background 'mode-line "black")
	(set-face-foreground 'mode-line "white")
	(set-face-foreground 'which-func "cyan")
	(setq smartrep-mode-line-active-bg "forest green")
	(set-face-background 'magit-item-highlight (face-attribute 'default :background))

	(when (require 'whitespace nil t)
		(global-whitespace-mode 1)
		(setq whitespace-style '(
														 face			;可視化
														 tabs			;タブ
														 spaces			;スペース
														 tab-mark		;タブ表示のマッピング
														 space-mark		;スペース表示のマッピング
														 ))
		(setq whitespace-space-regexp "\\(\x3000+\\)")
		(setq whitespace-display-mappings
					'((space-mark ?\x3000 [?\u25a1])
						(tab-mark	 ?\t	 [?\t])
						))
		(set-face-foreground	'whitespace-tab (face-attribute 'font-lock-comment-face :foreground))
		(set-face-background	'whitespace-tab (face-attribute 'default :background))
		(set-face-underline		'whitespace-tab t)
		(set-face-foreground	'whitespace-space (face-attribute 'font-lock-comment-face :foreground))
		(set-face-background	'whitespace-space (face-attribute 'default :background))
		(set-face-underline		'whitespace-space t)
		)

	(when linux-p
		(setq initial-frame-alist
					(append '(
										(width . 125) ; フレーム幅(文字数)
										(height . 38) ; フレーム高(文字数)
										)
									initial-frame-alist))
		;; デフォルト フォント
		(set-face-attribute 'default nil				:family "Migu 2M" :height 100)
		;; プロポーショナル フォント
		(set-face-attribute 'variable-pitch nil	:family "Migu 2M" :height 100)
		;; 等幅フォント
		(set-face-attribute 'fixed-pitch nil		:family "Migu 2M" :height 100)
		;; ツールチップ表示フォント
		(set-face-attribute 'tooltip nil				:family "Migu 2M" :height 100)
		)
	(when nt-p
		(setq initial-frame-alist
					(append '(
										(width . 125) ; フレーム幅(文字数)
										(height . 38) ; フレーム高(文字数)
										)
									initial-frame-alist))
		;; デフォルト フォント
		(set-face-attribute 'default nil				:family "Migu 1M" :height 100)
		;; プロポーショナル フォント
		(set-face-attribute 'variable-pitch nil	:family "Migu 1M" :height 100)
		;; 等幅フォント
		(set-face-attribute 'fixed-pitch nil		:family "Migu 1M" :height 100)
		;; ツールチップ表示フォント
		(set-face-attribute 'tooltip nil				:family "Migu 1M" :height 100)

		;; フォントサイズ調整
		(global-set-key (kbd "C-<wheel-up>")		'(lambda() (interactive) (text-scale-increase 1)))
		(global-set-key (kbd "C-=")							'(lambda() (interactive) (text-scale-increase 1)))
		(global-set-key (kbd "C-<wheel-down>")	'(lambda() (interactive) (text-scale-decrease 1)))
		(global-set-key (kbd "C--")							'(lambda() (interactive) (text-scale-decrease 1)))

		;; フォントサイズ リセット
		(global-set-key (kbd "M-0") '(lambda() (interactive) (text-scale-set 0)))
		)
)
