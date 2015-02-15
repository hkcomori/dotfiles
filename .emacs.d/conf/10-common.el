;-*- coding: utf-8; -*-
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
(setq next-line-add-newlines nil) ;;最終行でカーソル移動しても改行しない
(add-hook 'c-mode-common-hook '(lambda () (c-set-style "CC-MODE"))) ;;自動インデントのスタイルを変更
(global-font-lock-mode t)         ;プログラムの予約後に色をつける
(setq-default indent-tabs-mode t) ;インデントにタブ文字を使う
(setq make-backup-files nil)      ;*.~などのバックアップファイルを作らない
(setq auto-save-default nil)      ;.#*などのバックアップファイルを作らない
(setq require-final-newline t)    ;ファイルの最後に改行を挿入する
(put 'upcase-region 'disabled nil)		;リージョンの大文字変換を有効にする
(put 'downcase-region 'disabled nil)	;リージョンの小文字変換を有効にする

;;====================================
;; Tab Width
;;====================================
(setq-default tab-width 4)		  ;タブ幅の既定値を4にする
(add-hook 'emacs-lisp-mode-hook '(lambda () (setq tab-width 2)))

;;====================================
;; Extension
;;====================================
(defvar var-name nil)

;;====================================
;; Misc
;;====================================
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
(add-hook 'after-save-hook
					'executable-make-buffer-file-executable-if-script-p)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq line-number-mode t)		;行番号を表示
(setq column-number-mode nil)     ;列番号を表示
(which-function-mode 1)         ;カーソルがどの関数の中にあるかをモードラインに表示


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(setq truncate-lines nil)				;バッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)	;ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示

;; ファイル名が重複していたらディレクトリ名を追加する。
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*")
)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(show-paren-mode 1)			;対応する括弧を光らせる（グラフィック環境のみ作用）
(setq show-paren-style 'mixed)	;ウィンドウ内に収まらない時だけ括弧内も光らせる


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - tabbar                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;(when (require 'tabbar nil t)
;;  (call-interactively 'tabbar-mode t)			;tabbar有効化
;;  (setq tabbar-use-images nil)					;タブバーに画像を使わない
;;  (setq tabbar-buffer-groups-function nil)		;タブグループを使用（t：有効，nil：無効）
;;  (setq tabbar-separator '(0.5))				;タブの表示間隔
;;  ;; タブ切替にマウスホイールを使用（0：有効，-1：無効）
;;  (call-interactively 'tabbar-mwheel-mode -1)
;;  (remove-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
;;  (remove-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)
;;  ;; 左に表示されるボタンを無効化
;;  (dolist (btn '(tabbar-buffer-home-button
;;				 tabbar-scroll-left-button
;;				 tabbar-scroll-right-button))
;;	(set btn (cons (cons "" nil)
;;				   (cons "" nil))))
;;  (set-face-attribute
;;   'tabbar-default nil					;バーの背景
;;   :family (face-attribute 'default :family)
;;   :background (face-attribute 'mode-line :background)
;;   :height 1.0)
;;  (set-face-attribute
;;   'tabbar-selected nil					;アクティブなタブ
;;   :background (face-attribute 'mode-line :foreground)
;;   :foreground (face-attribute 'mode-line :background)
;;   :box nil)
;;  (set-face-attribute
;;   'tabbar-unselected nil				;非アクティブなタブ
;;   :background (face-attribute 'mode-line :background)
;;   :foreground (face-attribute 'mode-line :foreground)
;;   :box nil)
;;  ;; キーバインド
;;  (global-set-key (kbd "M-<right>") 'tabbar-forward-tab) ;次のタブ
;;  (global-set-key (kbd "M-<left>") 'tabbar-backward-tab) ;前のタブ
;;)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - migemo                                               ;;;
;;;   https://github.com/emacs-jp/migemo                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(when (and (executable-find "cmigemo") (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (when nt-p (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  (when linux-p (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
  (defvar migemo-user-dictionary nil)
  (defvar migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ server                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; emacs-server起動
(require 'server)
(defun server-ensure-safe-dir (dir) "Noop" t)
(setq server-socket-dir "~/.emacs.d")
(unless (server-running-p)
  (server-start)
)
