;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ frame                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(modify-frame-parameters nil '((wait-for-wm . nil)))	;GUI起動を高速化

(menu-bar-mode -1)							;メニューバーを非表示
(tool-bar-mode -1)							;ツールバーを非表示
(scroll-bar-mode -1)						;スクロールバーを非表示

(setq-default cursor-in-non-selected-windows t)				;非アクティブウィンドウのカーソル表示

(setq frame-title-format "%f - Emacs")	;フレームタイトル
(add-to-list 'initial-frame-alist						 '(width       . 125))				;フレーム幅
(add-to-list 'initial-frame-alist						 '(height      . 38))				;フレーム高さ
(add-to-list 'initial-frame-alist						 '(left-fringe . 0 ))				;左フリンジ幅
(add-to-list 'initial-frame-alist						 '(cursor-type . box))				;カーソル種別
(add-to-list 'initial-frame-alist						 '(alpha       . 100))				;透明度

;(define-key global-map (kbd "C-z c")						'make-frame-command)		;新しいフレームを開く

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フォントの設定                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set-face-attribute 'default nil				:family "Migu 1M" :height 100)	;通常フォント
(set-face-attribute 'variable-pitch nil	:family "Migu 1M" :height 100)	;プロポーショナル フォント
(set-face-attribute 'fixed-pitch nil		:family "Migu 1M" :height 100)	;等幅フォント
(set-face-attribute 'tooltip nil				:family "Migu 1M" :height 100)	;ツールチップ表示フォント

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 色の定義                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-color-white				"#F4F4F4")
(defvar my-color-black				"#262626")
(defvar my-color-gray					"#707070")	(defvar my-color-darkgray			"#505050")
(defvar my-color-red					"#FF5960")	(defvar my-color-darkred			"#BF1920")
(defvar my-color-green				"#40FF59")	(defvar my-color-darkgreen		"#009F00")
(defvar my-color-yellow				"#FFFF60")	(defvar my-color-daryellow		"#5F5F00")
(defvar my-color-cyan					"#40FFFF")	(defvar my-color-darkcyan			"#00BFBF")
(defvar my-color-blue					"#7199FF")	(defvar my-color-darkblue			"#5179DF")
(defvar my-color-purple				"#FF60FF")	(defvar my-color-darkpurple		"#BF20BF")
(defvar my-color-magenta			"#C080FF")	(defvar my-color-darkmagenta	"#8040BF")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 配色設定                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 全体の配色
(set-face-attribute 'default nil :foreground my-color-white :background my-color-black)

;; Syntaxハイライトに使用する文字色
(set-face-attribute 'font-lock-comment-face nil				:foreground my-color-gray)			;コメント
(set-face-attribute 'font-lock-builtin-face nil				:foreground my-color-magenta)		;組み込み関数
(set-face-attribute 'font-lock-string-face nil				:foreground my-color-red)				;文字列
(set-face-attribute 'font-lock-keyword-face nil				:foreground my-color-purple)		;キーワード
(set-face-attribute 'font-lock-type-face nil					:foreground my-color-green)			;型名
(set-face-attribute 'font-lock-variable-name-face nil	:foreground my-color-yellow)		;変数名
(set-face-attribute 'font-lock-constant-face nil			:foreground my-color-cyan)			;定数名
(set-face-attribute 'font-lock-function-name-face nil	:foreground my-color-blue)			;関数名

;; 編集領域の配色
(set-face-attribute 'region nil												:background my-color-darkgray)	;選択範囲の背景色
(set-face-attribute 'secondary-selection nil					:background my-color-darkgray)	;vhl/default-faceに継承
(set-face-attribute 'show-paren-match nil							:background my-color-darkgreen)	;対応する括弧がある場合
(set-face-attribute 'show-paren-mismatch nil					:background my-color-red)				;対応する括弧がない場合

;; その他UIの配色
(set-face-attribute 'fringe nil			:background (face-attribute 'default :background))	;フリンジ(左右の余白)
(set-face-attribute 'mode-line nil																											;モードライン
										:foreground (face-attribute 'default :background)										;文字色
										:background (face-attribute 'default :foreground))									;背景色
(set-face-attribute 'which-func nil	:foreground my-color-darkblue)											;モードラインに表示する関数名

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 空白文字の表示                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style '(face				;可視化
												 tabs				;タブ
												 spaces			;スペース
												 tab-mark		;タブ表示のマッピング
												 space-mark	;スペース表示のマッピング
												 ))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings '((space-mark ?\x3000 [?\u25a1])
																		(tab-mark	 ?\t	 [?\t])))
(set-face-attribute 'whitespace-tab nil
										:foreground (face-attribute 'font-lock-comment-face :foreground)
										:background (face-attribute 'default :background)
										:underline t)
(set-face-attribute 'whitespace-space nil
										:foreground (face-attribute 'font-lock-comment-face :foreground)
										:background (face-attribute 'default :background)
										:underline t)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 102-gui.el ends here
