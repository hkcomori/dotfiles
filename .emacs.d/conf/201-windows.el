;-*- coding: utf-8; -*-
;;; Code:
(when (eq system-type 'cygwin)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ language - coding system                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(set-default-coding-systems 'cp932-dos)				;デフォルトの文字コード
	(prefer-coding-system 'cp932-dos)							;テキストファイル／新規バッファの文字コード
	(set-file-name-coding-system 'utf-8-unix)			;ファイル名の文字コード
	(set-keyboard-coding-system 'utf-8-unix)			;キーボード入力の文字コード
	(set-language-environment 'utf-8)							;ターミナルで日本語入力を可能にする

	;; サブプロセスのデフォルト文字コード
	(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ shell                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; shellの存在を確認
	(defun skt:shell ()
		(or (executable-find "zsh")
				(executable-find "bash")
				(executable-find "cmdproxy")
				(error "can't find 'shell' command in PATH!!")))

	;; shell名の設定
	(setq shell-file-name (skt:shell))
	(setenv "SHELL" shell-file-name)
	(setq explicit-shell-file-name shell-file-name)

	(modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ language - input method                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; モードラインの表示文字列
	(setq-default w32-ime-mode-line-state-indicator "[Aa] ")
	(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
	;; デフォルトIME
	(setq default-input-method "W32-IME")
	;; IME初期化
	(w32-ime-initialize)
	;; バッファ切り替え時の状態引継ぎ設定
	(setq w32-ime-buffer-switch-p nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; @ screen - cursor                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; IME無効／有効時のカーソルカラー定義
	(defface cursor-ime-off '((t ((
											:foreground (face-attribute 'default :background)
											:background (face-attribute 'default :foreground))))) nil)
	(defface cursor-ime-on '((t ((
											:foreground (face-attribute 'default :background)
											:background my-color-red)))) nil)

	;; IME無効／有効時にカーソルカラーを切り替える
	(add-hook 'input-method-inactivate-hook
	 '(lambda() (set-face-attribute 'cursor nil
																	:foreground (face-attribute 'cursor-ime-off :foreground)
																	:background (face-attribute 'cursor-ime-off :background))))
	(add-hook 'input-method-activate-hook
	 '(lambda() (set-face-attribute 'cursor nil
																	:foreground (face-attribute 'cursor-ime-on :foreground)
																	:background (face-attribute 'cursor-ime-on :background))))
)																				;system-type

;; Local Variables:
;; coding: utf-8
;; End:

;;; 201-windows.el ends here
