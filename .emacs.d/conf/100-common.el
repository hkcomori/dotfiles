;-*- coding: utf-8; -*-
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ general                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 初期画面の非表示
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(set-language-environment "Japanese")						;言語環境は日本語
(setq read-file-name-completion-ignore-case t)	;ファイル名補完には大文字・小文字の区別をしない
(auto-compression-mode t)												;圧縮されたファイルも編集＆日本語infoの文字化け防止
(fset 'yes-or-no-p 'y-or-n-p)										;"yes or no"を"y or n"にする
(icomplete-mode 1)															;コマンドの補完候補を常に表示

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ edit                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq next-line-add-newlines nil)								;最終行でカーソル移動しても改行しない
(global-font-lock-mode t)												;プログラムの予約後に色をつける
(setq-default indent-tabs-mode t)								;インデントにタブ文字を使う
(setq-default tab-width 4)											;デフォルトのタブ幅を4にする
(setq require-final-newline t)									;ファイルの最後に改行を挿入する
(put 'upcase-region 'disabled nil)							;リージョンの大文字変換を有効にする
(put 'downcase-region 'disabled nil)						;リージョンの小文字変換を有効にする
(cua-mode t)																		;矩形選択を有効化
(setq cua-enable-cua-keys nil)									;C-xが切り取りになってしまうのを無効化

(define-key global-map (kbd "C-c a")			'align)									;アラインメント
(define-key global-map (kbd "C-h")				'delete-backward-char)	;前の文字を消す
(define-key global-map (kbd "M-d")				'delete-word)						;次の単語を消す
(define-key global-map (kbd "M-h")				'backward-delete-word)	;前の単語を消す
(define-key global-map (kbd "C-m")				'newline-and-indent)		;改行と同時にインデント

;; 使わないキーバインドの無効化
(define-key global-map [zenkaku-hankaku]	'toggle-input-method)		;IMEをオン/オフ
(define-key global-map [M-kanji]					'ignore)								;何もしない
(define-key global-map [kanji]						'ignore)								;何もしない
(define-key global-map (kbd "C-z")				'ignore)								;何もしない

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ file                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-auto-revert-mode 1)						;変更されたファイルを自動的に再読み込みする
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)	;保存時に実行権限を付与
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ file -lockfile                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ロックファイルの生成を抑止
;;(setq create-lockfiles nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ scroll                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scroll-preserve-screen-position t)	;スクロール時のカーソル位置を維持
(setq scroll-margin 5)										;スクロール開始の残り行数
(setq scroll-conservatively 1)						;スクロール時の行数
(setq next-screen-context-lines 5)				;画面スクロール時の重複表示する行数
(setq redisplay-dont-pause t)							;キー入力中の画面更新を抑止

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ file - backup                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルオープン時のバックアップ(~)
(setq make-backup-files t)						;自動バックアップの実行有無
(setq version-control t)							;バックアップファイルへの番号付与
(setq kept-new-versions 3)						;最新バックアップファイルの保持数
(setq kept-old-versions 0)						;最古バックアップファイルの保持数
(setq delete-old-versions t)					;バックアップファイル削除の実行有無

(setq backup-inhibited nil)						;編集中ファイルの自動バックアップする
(setq delete-auto-save-files nil)			;終了時に自動バックアップファイルを削除しない

(setq auto-save-list-file-name nil)		;編集中ファイルのバックアップをしない
(setq auto-save-list-file-prefix nil)	;編集中ファイルのバックアップをしない
(setq auto-save-timeout 3)						;編集中ファイルのバックアップ間隔(秒)
(setq auto-save-interval 100)					;編集中ファイルのバックアップ間隔(打鍵回数)

;; ファイルオープン時のバックアップ(~)の格納ディレクトリ
(setq backup-directory-alist
			(cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
						backup-directory-alist))

;; 編集中ファイル(##)の格納ディレクトリ
(setq auto-save-file-name-transforms
			`((".*" ,(expand-file-name "/tmp/emacsbk") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ window                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq truncate-lines nil)								;折り返し表示
(setq truncate-partial-width-windows t)	;ウィンドウ縦分割時は折り返さない

;; モードライン
(setq line-number-mode t)								;行番号を表示
(setq column-number-mode nil)						;列番号を表示
(which-function-mode 1)									;カーソルがどの関数の中にあるかをモードラインに表示

;; ウィンドウ間の移動
(require 'windmove)
(setq windmove-wrap-around t)						;バッファ移動をShift+矢印で
(windmove-default-keybindings)					;バッファ移動をShift+矢印で

;; ediff
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)	; コントロール用のウィンドウを同一フレーム内に表示
(setq ediff-split-window-function 'split-window-horizontally)	; diffのウィンドウを上下ではなく左右に並べる

;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; 対応する括弧の強調
(require 'paren)
(show-paren-mode 1)											;対応する括弧を光らせる(CUIモード時は無効)
(setq show-paren-style 'mixed)					;ウィンドウ内に収まらない時だけ括弧内も光らせる

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ buffer                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq initial-scratch-message "")				;scratchのメッセージを消す

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ window - cursor                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq blink-cursor-interval 0.5)				;カーソルの点滅間隔を0.5秒
(setq blink-cursor-delay 60.0)					;カーソルの点滅を1分間停止
(blink-cursor-mode 1)										;カーソルの点滅を有効化

;; カーソル移動
(define-key global-map (kbd "C-x :")						'goto-line)							;指定行に移動する
(define-key global-map (kbd "C-M-<right>")			'forward-sexp)					;次のS式
(define-key global-map (kbd "C-M-<left>")				'backward-sexp)					;前のS式

;; マウス操作
(define-key minibuffer-inactive-mode-map [mouse-1]	'ignore)
(define-key global-map [C-down-mouse-1]					'mouse/mark-current-sexp)
(define-key global-map [C-mouse-1]							'mouse/mark-current-sexp)
(define-key global-map [C-double-mouse-1]				'mouse/mark-current-sexp)

;; カーソル位置の保存
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".emacs-places"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ search - grep                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'grep)
(cond
	((eq system-type 'gnu/linux)
	 (grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Au8 -Ia  {} +" . 40))
	 (grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
	)
	((eq system-type 'cygwin)
	 (grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Asjis -Ia  {} +" . 42))
	 (grep-apply-setting 'grep-command "lgrep -n -Asjis -Ia ")
	 )
	)

;; grep結果の編集
(require 'wgrep)
(define-key grep-mode-map "e" 'wgrep-change-to-wgrep-mode)	;"e"キーで編集モードに
(setq wgrep-auto-save-buffer t)															;編集完了と同時に保存
(setq wgrep-enable-key "r")																	;"r"キーで編集モードに

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ search - isearch                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)	;C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)		;C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)	;C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)		;Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "C-g")												;C-gで検索を終了
	'(lambda() (interactive) (isearch-done)))

;; サクラエディタとの互換キーバインド
(define-key global-map [f3]		'isearch-forward)
(define-key global-map [S-f3]	'isearch-backward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ server                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsclientを使うための設定
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")					;socket-fileの格納場所
(unless (server-running-p) (server-start))		;serverが起動していなければ起動する

;; server使用時にバッファを閉じる際の確認メッセージを表示しない
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 100-common.el ends here
