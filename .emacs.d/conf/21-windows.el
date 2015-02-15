;-*- coding: utf-8; -*-
(when nt-p
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shell                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;; setting for japanese
  (set-terminal-coding-system 'cp932)
  (set-keyboard-coding-system 'cp932)
  (set-buffer-file-coding-system 'cp932-dos)
  (setq file-name-coding-system 'cp932)
  (setq default-buffer-file-coding-system 'cp932)
  (prefer-coding-system 'cp932-dos)    ; 文字コード認識優先順位1
  (prefer-coding-system 'utf-8)        ; 文字コード認識優先順位2
  (set-default-coding-systems 'cp932)


  (if (and (require 'shell nil t) (executable-find "zsh"))
			(lambda ()
				(setq shell-file-name "zsh")
				(setenv "SHELL" shell-file-name)
				(setq explicit-shell-file-name shell-file-name)
				(setq multi-term-program shell-file-name)
				)
		(lambda ()
			(setq explicit-shell-file-name "bash.exe")
			(setq shell-command-switch "-c")
			(setq shell-file-name "bash.exe"))
		)

  (modify-coding-system-alist 'process ".*sh\\.exe" 'utf-8)


  ;; YaTeX-mode
  ;; (setq auto-mode-alist  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  ;; (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  ;; (setq load-path (cons "d:/users/thyobu/dropbox/settings/windows/yatex" load-path))
  ;; (setq tex-command "platex")
  ;; (setq dvi2-command "c:/w32tex/dviout/dviout")
  ;; (setq dviprint-command-format "dvipdfmx %s ")

;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ language - coding system                                      ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;(set-default-coding-systems 'cp932-dos) ;デフォルトの文字コード
  ;;(prefer-coding-system 'cp932-dos)		;テキストファイル／新規バッファの文字コード
  ;;(set-file-name-coding-system 'utf-8-unix) ;ファイル名の文字コード
  ;;(set-keyboard-coding-system 'utf-8-unix) ;キーボード入力の文字コード
  ;;
;;;; サブプロセスのデフォルト文字コード
  ;;(setq default-process-coding-system '(undecided-dos . utf-8-unix))
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ language - input method                                       ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; モードラインの表示文字列
  ;;(setq-default w32-ime-mode-line-state-indicator "[Aa] ")
  ;;(setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  ;;
;;;; IME初期化
  ;;(w32-ime-initialize)
  ;;
;;;; デフォルトIME
  ;;(setq default-input-method "W32-IME")
  ;;
;;;; IME変更
  ;;(global-set-key (kbd "C-\\") 'toggle-input-method)
  ;;
;;;; 漢字/変換キー入力時のエラーメッセージ抑止
  ;;(global-set-key (kbd "<A-kanji>") 'ignore)
  ;;(global-set-key (kbd "<M-kanji>") 'ignore)
  ;;(global-set-key (kbd "<kanji>") 'ignore)
  ;;
  ;;
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ screen - frame                                                ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
  ;;(setq default-frame-alist
  ;;      (append '((width                . 85)  ; フレーム幅
  ;;                (height               . 38 ) ; フレーム高
  ;;             ;; (left                 . 70 ) ; 配置左位置
  ;;             ;; (top                  . 28 ) ; 配置上位置
  ;;                (line-spacing         . 0  ) ; 文字間隔
  ;;                (left-fringe          . 10 ) ; 左フリンジ幅
  ;;                (right-fringe         . 11 ) ; 右フリンジ幅
  ;;                (menu-bar-lines       . 1  ) ; メニューバー
  ;;                (tool-bar-lines       . 1  ) ; ツールバー
  ;;                (vertical-scroll-bars . 1  ) ; スクロールバー
  ;;                (scroll-bar-width     . 17 ) ; スクロールバー幅
  ;;                (cursor-type          . box) ; カーソル種別
  ;;                (alpha                . 100) ; 透明度
  ;;                ) default-frame-alist) )
  ;;(setq initial-frame-alist default-frame-alist)
  ;;
;;;; フレーム タイトル
  ;;(setq frame-title-format
  ;;      '("emacs " emacs-version (buffer-file-name " - %f")))
  ;;
;;;; 初期画面の非表示
  ;;(setq inhibit-startup-message nil)
  ;;(setq inhibit-startup-screen nil)
  ;;
;;;; フルスクリーン化
  ;;(global-set-key (kbd "<A-return>") 'toggle-frame-fullscreen)
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ screen - mode line                                            ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; 行番号の表示
  ;;(line-number-mode t)
  ;;
;;;; 列番号の表示
  ;;(column-number-mode t)
  ;;
;;;; モードライン カスタマイズ
  ;;(setq-default
  ;; mode-line-format
  ;; `(
  ;;   ""
  ;;   w32-ime-mode-line-state-indicator
  ;;   " "
  ;;   mode-line-mule-info
  ;;   mode-line-modified
  ;;   mode-line-frame-identification
  ;;   mode-line-buffer-identification
  ;;   " "
  ;;   global-mode-string
  ;;   " %[("
  ;;   mode-name
  ;;   mode-line-process
  ;;   "%n"
  ;;   ")%] "
  ;;   (which-func-mode ("" which-func-format " "))
  ;;   (line-number-mode
  ;;    (:eval
  ;;     (format "L%%l/L%d " (count-lines (point-max) 1) )))
  ;;   (column-number-mode " C%c ")
  ;;   (-3 . "%p")
  ;;   )
  ;; )
  ;;(setq mode-line-frame-identification " ")
  ;;
;;;; cp932エンコードの表記変更
  ;;(coding-system-put 'cp932 :mnemonic ?P)
  ;;(coding-system-put 'cp932-dos :mnemonic ?P)
  ;;(coding-system-put 'cp932-unix :mnemonic ?P)
  ;;(coding-system-put 'cp932-mac :mnemonic ?P)
  ;;
;;;; UTF-8エンコードの表記変更
  ;;(coding-system-put 'utf-8 :mnemonic ?U)
  ;;(coding-system-put 'utf-8-with-signature :mnemonic ?u)
  ;;
;;;; 改行コードの表記追加
  ;;(setq eol-mnemonic-dos       ":Dos ")
  ;;(setq eol-mnemonic-mac       ":Mac ")
  ;;(setq eol-mnemonic-unix      ":Unx ")
  ;;(setq eol-mnemonic-undecided ":??? ") 
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ screen - buffer                                               ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; バッファ画面外文字の切り詰め表示
  ;;(setq truncate-lines nil)
  ;;
;;;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
  ;;(setq truncate-partial-width-windows t)
  ;;
;;;; 同一バッファ名にディレクトリ付与
  ;;(require 'uniquify)
  ;;(setq uniquify-buffer-name-style 'forward)
  ;;(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  ;;(setq uniquify-ignore-buffers-re "*[^*]+*")
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ screen - cursor                                               ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; カーソルの点滅
  ;;(blink-cursor-mode 0)
  ;;
;;;; 非アクティブウィンドウのカーソル表示
  ;;(setq-default cursor-in-non-selected-windows t)
  ;;
;;;; IME無効／有効時のカーソルカラー定義
  ;;(unless (facep 'cursor-ime-off)
  ;;  (make-face 'cursor-ime-off)
  ;;  (set-face-attribute 'cursor-ime-off nil
  ;;                      :background "DarkRed" :foreground "White")
  ;;  )
  ;;(unless (facep 'cursor-ime-on)
  ;;  (make-face 'cursor-ime-on)
  ;;  (set-face-attribute 'cursor-ime-on nil
  ;;                      :background "DarkGreen" :foreground "White")
  ;;  )
  ;;
;;;; IME無効／有効時のカーソルカラー設定
  ;;(add-hook
  ;; 'input-method-inactivate-hook
  ;; '(lambda()
  ;;    (if (facep 'cursor-ime-off)
  ;;        (let ( (fg (face-attribute 'cursor-ime-off :foreground))
  ;;               (bg (face-attribute 'cursor-ime-off :background)) )
  ;;          (set-face-attribute 'cursor nil :foreground fg :background bg)
  ;;          )
  ;;      )
  ;;    )
  ;; )
  ;;(add-hook
  ;; 'input-method-activate-hook
  ;; '(lambda()
  ;;    (if (facep 'cursor-ime-on)
  ;;        (let ( (fg (face-attribute 'cursor-ime-on :foreground))
  ;;               (bg (face-attribute 'cursor-ime-on :background)) )
  ;;          (set-face-attribute 'cursor nil :foreground fg :background bg)
  ;;          )
  ;;      )
  ;;    )
  ;; )
  ;;
;;;; バッファ切り替え時の状態引継ぎ設定
  ;;(setq w32-ime-buffer-switch-p nil)
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ search - isearch                                              ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; 大文字・小文字を区別しないでサーチ
  ;;(setq-default case-fold-search nil)
  ;;
;;;; インクリメント検索時に縦スクロールを有効化
  ;;(setq isearch-allow-scroll nil)
  ;;
;;;; C-dで検索文字列を一文字削除
  ;;(define-key isearch-mode-map (kbd "C-d") 'isearch-delete-char)
  ;;
;;;; C-yで検索文字列にヤンク貼り付け
  ;;(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-kill)
  ;;
;;;; C-eで検索文字列を編集
  ;;(define-key isearch-mode-map (kbd "C-e") 'isearch-edit-string)
  ;;
;;;; Tabで検索文字列を補完
  ;;(define-key isearch-mode-map (kbd "TAB") 'isearch-yank-word)
  ;;
;;;; C-gで検索を終了
  ;;(define-key isearch-mode-map (kbd "C-g")
  ;;  '(lambda() (interactive) (isearch-done)))
  ;;
;;;; 日本語の検索文字列をミニバッファに表示
  ;;(define-key isearch-mode-map (kbd "<compend>")
  ;;  '(lambda() (interactive) (isearch-update)))
  ;;(define-key isearch-mode-map (kbd "<kanji>")
  ;;  'isearch-toggle-input-method)
  ;;(add-hook
  ;; 'isearch-mode-hook
  ;; '(lambda() (setq w32-ime-composition-window (minibuffer-window)))
  ;; )
  ;;(add-hook
  ;; 'isearch-mode-end-hook
  ;; '(lambda() (setq w32-ime-composition-window nil))
  ;; )
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ file - backup                                                 ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; ファイルオープン時のバックアップ（~）
  ;;(setq make-backup-files   t)  ;; 自動バックアップの実行有無
  ;;(setq version-control     t)  ;; バックアップファイルへの番号付与
  ;;(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
  ;;(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
  ;;(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無
  ;;
;;;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
  ;;(setq backup-directory-alist
  ;;      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
  ;;            backup-directory-alist))
  ;;
;;;; 編集中ファイルの自動バックアップ
  ;;(setq backup-inhibited nil)
  ;;
;;;; 終了時に自動バックアップファイルを削除
  ;;(setq delete-auto-save-files nil)
  ;;
;;;; 編集中ファイルのバックアップ
  ;;(setq auto-save-list-file-name nil)
  ;;(setq auto-save-list-file-prefix nil)
  ;;
;;;; 編集中ファイルのバックアップ間隔（秒）
  ;;(setq auto-save-timeout 3)
  ;;
;;;; 編集中ファイルのバックアップ間隔（打鍵）
  ;;(setq auto-save-interval 100)
  ;;
;;;; 編集中ファイル（##）の格納ディレクトリ
  ;;(setq auto-save-file-name-transforms
  ;;      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ file - lockfile                                               ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; ロックファイルの生成を抑止
  ;;(setq create-lockfiles nil)
  ;;
  ;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;;;; @ scroll                                                        ;;;
;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
  ;;
;;;; スクロール時のカーソル位置を維持
  ;;(setq scroll-preserve-screen-position t)
  ;;
;;;; スクロール開始の残り行数
  ;;(setq scroll-margin 0)
  ;;
;;;; スクロール時の行数
  ;;(setq scroll-conservatively 10000)
  ;;
;;;; スクロール時の行数（scroll-marginに影響せず）
  ;;(setq scroll-step 0)
  ;;
;;;; 画面スクロール時の重複表示する行数
  ;;(setq next-screen-context-lines 1)
  ;;
;;;; キー入力中の画面更新を抑止
  ;;(setq redisplay-dont-pause t)
  ;;
;;;; recenter-top-bottomのポジション
  ;;(setq recenter-positions '(top bottom))
  ;;
;;;; 横スクロール開始の残り列数
  ;;(setq hscroll-margin 1)
  ;;
;;;; 横スクロール時の列数
  ;;(setq hscroll-step 1)
  ;;
;;;; スクロールダウン
  ;;(global-set-key (kbd "C-z") 'scroll-down)
  ;;
;;;; バッファの最後までスクロールダウン
  ;;(defadvice scroll-down (around scroll-down activate compile)
  ;;  (interactive)
  ;;  (let (
  ;;        (bgn-num (+ 1 (count-lines (point-min) (point))))
  ;;        )
  ;;    (if (< bgn-num (window-height))
  ;;        (goto-char (point-min))
  ;;      ad-do-it) ))
  ;;
;;;; バッファの先頭までスクロールアップ
  ;;(defadvice scroll-up (around scroll-up activate compile)
  ;;  (interactive)
  ;;  (let (
  ;;        (bgn-num (+ 1 (count-lines (point-min) (point))))
  ;;        (end-num nil)
  ;;        )
  ;;    (save-excursion
  ;;      (goto-char (point-max))
  ;;      (setq end-num (+ 1 (count-lines (point-min) (point))))
  ;;      )
  ;;    (if (< (- (- end-num bgn-num) (window-height)) 0)
  ;;        (goto-char (point-max))
  ;;      ad-do-it) ))
)
