;;-*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パッケージ管理                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'package nil t)
	;; Add package-archives
	(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
	;; Initialize
	;; (package-initialize)
)

;; melpa.el
(when (require 'melpa nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'anything nil t)
	(global-set-key (kbd "C-x C-b") 'anything-for-files)
	(global-set-key (kbd "M-y") 'anything-show-kill-ring)
	(setq
	 anything-idle-delay 0.1							;候補表示までの待ち時間
	 anything-input-idle-delay 0.1				;再描画までの待ち時間
	 anything-candidate-number-limit 100	;候補の最大数
	 anything-enable-shortcuts 'alphabet	;候補選択ショートカットをアルファベットに
	 anything-quick-update t							;候補が多い時に体感速度を早くする
	 )
	(when (and (executable-find "cmigemo") (require 'migemo nil t))
		(require 'anything-migemo nil t)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルタイプ                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arduino
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;; MATLAB
(when (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
	(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
	(setq auto-mode-alist (append '(("\\.m\\'" . matlab-mode)) auto-mode-alist))
	(add-hook 'matlab-mode-hook (lambda () (local-unset-key "\C-h")))
)

;; バイナリ
(setq auto-mode-alist (append '(("\\.dll\\'" . hexl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.exe\\'" . hexl-mode)) auto-mode-alist))

;; HTML 中の Java Script
(autoload 'javascript-mode "javascript" "JavaScript mode" t)
(when (require 'mmm-mode nil t)
	(setq mmm-global-mode 'maybe nil t)
	(setq mmm-submode-decoration-level 2)
	;; js in html
	(mmm-add-classes
	 '((js-in-html
			:submode javascript-mode
			:front "<script[^>]*>\n<!--\n"
			:back  "// ?-->\n</script>")))
	(mmm-add-mode-ext-class nil "\\.s?html?\\(\\..+\\)?$" 'js-in-html))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カーソル操作                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; カーソル履歴
(when (require 'point-undo nil t)
	(define-key global-map [f7] 'point-undo)
	(define-key global-map [S-f7] 'point-redo)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;矩形選択を有効化                                                                                   
(cua-mode t)                                                                                 
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化

(when (require 'auto-complete nil t)
	(defvar myext-auto-complete t)
	(defun my/ac-mode-enable ()
		(auto-complete-mode -1)
		(auto-complete-mode t))
	(defun my/ac-mode-disable ()
		(auto-complete-mode -1))
	(global-auto-complete-mode t)			;自動補完を常に有効
	(setq ac-auto-show-menu 0.3)			;候補が出るまでの時間 (default: 0.8)
)

(when (require 'multiple-cursors nil t)
	;; insert specific serial number
	(defvar my/mc/insert-numbers-hist nil)
	(defvar my/mc/insert-numbers-inc 1)
	(defvar my/mc/insert-numbers-pad "%01d")
	(defun my/mc/insert-numbers (start inc pad)
	"Insert increasing numbers for each cursor specifically."
	(interactive
	 (list (read-number "Start from: " 0)
				 (read-number "Increment by: " 1)
				 (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
	(setq mc--insert-numbers-number start)
	(setq my/mc/insert-numbers-inc inc)
	(setq my/mc/insert-numbers-pad pad)
	(mc/for-each-cursor-ordered
	 (mc/execute-command-for-fake-cursor
	  'my/mc--insert-number-and-increase
	  cursor)))
	(defun my/mc--insert-number-and-increase ()
		(interactive)
		(insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
		(setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))
	(defun mc/mark-next-like-this-and-cycle-forward ()
		(interactive)
		(mc/mark-next-like-this 1)
		(mc/cycle-forward))
	(defun mc/mark-previous-like-this-and-cycle-backward ()
		(interactive)
		(mc/mark-previous-like-this 1)
		(mc/cycle-backward))
	;; multiple-cursors使用時に一回のみ実行する関数の設定
	(add-to-list 'mc--default-cmds-to-run-once 'my/mc/insert-numbers)
	(add-to-list 'mc--default-cmds-to-run-once 'my/mc--insert-number-and-increase)
	(add-to-list 'mc--default-cmds-to-run-once 'mc/mark-next-like-this-and-cycle-forward)
	(add-to-list 'mc--default-cmds-to-run-once 'mc/mark-previous-like-this-and-cycle-backward)
	;; auto-completeと併用するための設定
	(when myext-auto-complete
		(add-hook 'multiple-cursors-mode-enabled-hook 'my/ac-mode-disable)
		(add-hook 'multiple-cursors-mode-disabled-hook 'my/ac-mode-enable)
		)
	(when (require 'smartrep nil t)
		;; smartrepによるコマンド実行中はキー入力をエコーしない
		;; http://shakenbu.org/yanagi/d/?date=20140105
		(defadvice smartrep-map-internal (around smartrep-silence-echo-keystrokes activate)
			(let ((echo-keystrokes 0)) ad-do-it))
		(declare-function smartrep-define-key "smartrep")
		(global-unset-key "\C-t")
		(smartrep-define-key global-map "C-t"
			'(("C-t" . 'mc/mark-next-like-this-and-cycle-forward)
				("n"   . 'mc/mark-next-like-this-and-cycle-forward)
				("p"   . 'mc/mark-previous-like-this-and-cycle-backward)
				("C-v" . 'mc/cycle-forward)
				("M-v" . 'mc/cycle-backward)
				("m"   . 'mc/mark-more-like-this-extended)
				("u"   . 'mc/unmark-next-like-this)
				("U"   . 'mc/unmark-previous-like-this)
				("s"   . 'mc/skip-to-next-like-this)
				("S"   . 'mc/skip-to-previous-like-this)
				("a"   . 'mc/mark-all-like-this)
				("d"   . 'mc/mark-all-like-this-dwim)
				("i"   . 'my/mc/insert-numbers)
				("o"   . 'mc/sort-regions)
				("O"   . 'mc/reverse-regions)))
		)
	(global-set-key (kbd "C-M-c") 'mc/edit-lines)
	(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 履歴                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルの履歴
(when (require 'recentf nil t)
	(when (require 'cl nil t)
		(defvar my-recentf-list-prev nil)
		(defadvice recentf-save-list
			(around no-message activate)
			"If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
			(unless (equal recentf-list my-recentf-list-prev)
				(cl-flet ((message (format-string &rest args)
													 (eval `(format ,format-string ,@args)))
									(write-file (file &optional confirm)
															(let ((str (buffer-string)))
																(with-temp-file file
																	(insert str)))))
					ad-do-it
					(setq my-recentf-list-prev recentf-list))))
		(defadvice recentf-cleanup
			(around no-message activate)
			"suppress the output from `message' to minibuffer"
			(cl-flet ((message (format-string &rest args)
												 (eval `(format ,format-string ,@args))))
				ad-do-it))
		)
	;;(setq recentf-exclude '("^\\.emacs\\.bmk$"))
	(setq recentf-max-menu-items 10)
	(setq recentf-max-saved-items 2000)
	(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
	(setq recentf-exclude '(".recentf"))
	(setq recentf-auto-cleanup 60)
	(setq recentf-auto-save-timer (run-with-idle-timer 900 t 'recentf-save-list))
	(recentf-mode t)
)

;; undo履歴をツリー表示
(when (require 'undo-tree nil t)
	(global-undo-tree-mode)
	(setq undo-tree-auto-save-history t)
	(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree"))))
	(setq undo-limit 600000)
	(setq undo-strong-limit 900000)
	(global-set-key (kbd "C-/") 'undo-tree-undo)
	(global-set-key (kbd "C-\\") 'undo-tree-redo)
)

;;カーソル位置の保存
(when (require 'saveplace nil t)
	(setq-default save-place t)
	(setq save-place-file (concat user-emacs-directory ".emacs-plases"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイル操作                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; diredを便利にする
(when (require 'dired-x nil t))

;; diredから"r"でファイル名をインライン編集する
(when (require 'wdired nil t)
	(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grep                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; grepの日本語対応
(when (require 'grep nil t)
	(when linux-p
		(grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Au8 -Ia  {} +" . 40))
		(grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
		)
	(when nt-p
		(grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Asjis -Ia  {} +" . 42))
		(grep-apply-setting 'grep-command "lgrep -n -Asjis -Ia ")
		)
)

;; grep結果を編集可能にする
(when (require 'wgrep nil t)
	(define-key grep-mode-map "e" 'wgrep-change-to-wgrep-mode)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Magit
(when (require 'magit nil t)
	(define-key mode-specific-map "m" 'magit-status)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 端末                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'multi-term nil t)
	(setq multi-term-program shell-file-name)
	(defun toggle-term-view () (interactive)
		(cond ((eq major-mode 'term-mode)
					 (fundamental-mode)
					 (view-mode-enable)
					 (local-set-key (kbd "C-c C-c") 'toggle-term-view)
					 (setq multi-term-cursor-point (point)))
					((eq major-mode 'fundamental-mode)
					 (view-mode-disable)
					 (goto-char multi-term-cursor-point)
					 (multi-term-internal))))
	(add-hook 'term-mode-hook
						(lambda ()
							(define-key term-raw-map (kbd "C-y") 'term-paste)
							(define-key term-raw-map (kbd "C-h") 'term-send-backspace)
							(define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
							(define-key term-raw-map (kbd "C-r") 'term-send-reverse-search-history)
							(define-key term-raw-map (kbd "C-s") 'term-send-reverse-search-history)
							(define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
							(define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
							(define-key term-raw-map (kbd "C-v") nil)
							(define-key term-raw-map (kbd "ESC ESC") 'term-send-raw)
							(define-key term-raw-map (kbd "C-q") 'toggle-term-view)
							(define-key term-raw-map (kbd "C-DEL") 'term-send-forward-kill-word)
							(define-key term-raw-map (kbd "C-<backspace>") 'term-send-backward-kill-word)
							(define-key term-raw-map (kbd "M-<right>") 'term-send-forward-word)
							(define-key term-raw-map (kbd "C-<right>") 'term-send-forward-word)
							(define-key term-raw-map (kbd "M-<left>") 'term-send-backward-word)
							(define-key term-raw-map (kbd "C-<left>") 'term-send-backward-word)
							))
	;;(add-hook 'emacs-startup-hook (multi-term))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 関数一覧
(when (require 'summarye nil t)
	(define-key mode-specific-map "l" 'se/make-summary-buffer)
	)

;; howm
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
(setq howm-file-name-format "%Y-%m.howm")
(add-to-list 'load-path "elisp/howm")
(when (require 'howm nil t)
	(global-set-key "\C-c,," 'howm-menu)
)

;; Yasnippet
(when (require 'yasnippet nil t)
	(yas-global-mode 1)
	(custom-set-variables '(yas-trigger-key "TAB"))
	(add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1) ))
	(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
	(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
	(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
)
