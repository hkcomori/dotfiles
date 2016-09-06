;-*- coding: utf-8; -*-
;;; Code:

;; Emacs 23より前のバージョンでは
;; user-emacs-directory変数が未定義のため次の設定を追加
(when (< emacs-major-version 23)
	(defvar user-emacs-directory "~/.emacs.d"))

(set-language-environment "Japanese")

;; ファイル名補完には大文字・小文字の区別をしない
(setq read-file-name-completion-ignore-case t)

;;====================================
;; Edit
;;====================================
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; 編集                                                             ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq next-line-add-newlines nil)																				;;最終行でカーソル移動しても改行しない
(add-hook 'c-mode-common-hook '(lambda () (c-set-style "CC-MODE")))			;;自動インデントのスタイルを変更
(global-font-lock-mode t)																								;プログラムの予約後に色をつける
(setq-default indent-tabs-mode t)																				;インデントにタブ文字を使う
(setq require-final-newline t)																					;ファイルの最後に改行を挿入する
(put 'upcase-region 'disabled nil)																			;リージョンの大文字変換を有効にする
(put 'downcase-region 'disabled nil)																		;リージョンの小文字変換を有効にする
(cua-mode t)														; 矩形選択を有効化
(setq cua-enable-cua-keys nil)	; そのままだと C-x が切り取りになってしまったりするので無効化

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 履歴                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;カーソル位置の保存
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory ".emacs-places"))

;;====================================
;; Tab Width
;;====================================
(setq-default tab-width 4)
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq tab-width 2)))

;;====================================
;; Misc
;;====================================
(require 'windmove)
(setq windmove-wrap-around t)         ;バッファ移動をShift+矢印で
(windmove-default-keybindings)        ;バッファ移動をShift+矢印で
(setq inhibit-startup-message t)      ;起動時のメッセージを消す
(auto-compression-mode t)             ;圧縮されたファイルも編集＆日本語infoの文字化け防止
(fset 'yes-or-no-p 'y-or-n-p)         ;"yes or no"を"y or n"にする
(icomplete-mode 1)                    ;コマンドの補完候補を常に表示
;; server使用時にバッファを閉じる際の確認メッセージを表示しない
(remove-hook
'kill-buffer-query-functions 
'server-kill-buffer-query-function)

;;====================================
;; File
;;====================================
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-auto-revert-mode 1)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)	;C-dで検索文字列を一文字削除
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)		;C-yで検索文字列にヤンク貼り付け
(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)	;C-eで検索文字列を編集
(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)		;Tabで検索文字列を補完
(define-key isearch-mode-map (kbd "C-g")												;C-gで検索を終了
	'(lambda() (interactive) (isearch-done)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq line-number-mode t)				;行番号を表示
(setq column-number-mode nil)		;列番号を表示
(which-function-mode 1)					;カーソルがどの関数の中にあるかをモードラインに表示

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq truncate-lines nil)								;折り返し表示
(setq truncate-partial-width-windows t)	;ウィンドウ縦分割時は折り返さない

(require 'ediff nil t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)	; コントロール用のバッファを同一フレーム内に表示
(setq ediff-split-window-function 'split-window-horizontally)	; diffのバッファを上下ではなく左右に並べる

;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(require 'paren)
(show-paren-mode 1)			;対応する括弧を光らせる（グラフィック環境のみ作用）
(setq show-paren-style 'mixed)	;ウィンドウ内に収まらない時だけ括弧内も光らせる

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;@ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ファイルオープン時のバックアップ（~）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
			(cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
						backup-directory-alist))

;; 編集中ファイルの自動バックアップ
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
			`((".*" ,(expand-file-name "/tmp/emacsbk") t)))
  

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ server                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; emacs-server起動
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")
(unless (server-running-p) (server-start))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 100-common.el ends here
