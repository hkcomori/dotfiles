(require 'whitespace)
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
