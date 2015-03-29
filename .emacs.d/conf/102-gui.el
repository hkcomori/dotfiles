;;; Code:
(when window-system
	(modify-frame-parameters nil '((wait-for-wm . nil)))	 ;;GUI起動を高速化
	(menu-bar-mode -1)							 ;;メニューバーを非表示
	(tool-bar-mode -1)							 ;;ツールバーを非表示
	(scroll-bar-mode -1)						 ;;スクロールバーを非表示
	(setq frame-title-format (list "%f - Emacs@" (system-name)))
	
	(set-face-background 'fringe (face-attribute 'default :background))
	(set-face-foreground 'font-lock-comment-face "gray")
	;; モードライン
	(set-face-background 'mode-line "black")
	(set-face-foreground 'mode-line "white")
	(set-face-foreground 'which-func "cyan")
	(setq smartrep-mode-line-active-bg "forest green")

	(cond ((eq system-type 'gnu/linux)
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
				 )															;system-type 'gnu/linux
				
				((eq system-type 'cygwin)
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
				 )															;system-type 'cygwin
				)																;cond
	)																			;window-system

;; Local Variables:
;; coding: utf-8
;; End:

;;; 102-gui.el ends here
