;;-*- coding: utf-8; -*-
(when (require 'package nil t)
	;; Add package-archives
	(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
	(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
	;; Initialize
	(package-initialize)
)

;; melpa.el
(when (require 'melpa nil t))

;; grepの日本語対応
(when (require 'grep nil t)
	(grep-apply-setting 'grep-find-command '("find . -type f -exec lgrep -n -Au8 -Ia  {} +" . 40))
	(grep-apply-setting 'grep-command "lgrep -n -Au8 -Ia ")
)

;; mozc
(when (require 'mozc nil t)
	(setq default-input-method "japanese-mozc")
	(setq mozc-candidate-style 'overlay)
)

; ウィンドウ管理モードを有効にする
(when (require 'e2wm nil t)
	(global-set-key (kbd "M-+") 'e2wm:start-management)
)

(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;; ファイルの履歴
(when (require 'recentf nil t)
	(recentf-mode t)
	;;(setq recentf-exclude '("^\\.emacs\\.bmk$"))
	(setq recentf-max-menu-items 10)
	(setq recentf-max-saved-items 50)
)

;; undo履歴を強化
(when (require 'undo-tree nil t)
	(global-undo-tree-mode)
	(setq undo-tree-auto-save-history t)
	(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree"))))
	(setq undo-limit 600000)
	(setq undo-strong-limit 900000)
	(global-set-key (kbd "C-/") 'undo-tree-undo)
	(global-set-key (kbd "C-\\") 'undo-tree-redo)
)

;; auto-complete
(when (require 'auto-complete nil t)
	(global-auto-complete-mode t)			;自動補完を常に有効
	(setq ac-auto-show-menu 0.3)			;候補が出るまでの時間 (default: 0.8)
	;;(when (require 'auto-complete-clang nil t)
	;;)
)

;; howm
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
(setq howm-file-name-format "%Y-%m.howm")
(add-to-list 'load-path "elisp/howm")
(when (require 'howm nil t)
	(global-set-key "\C-c,," 'howm-menu)
)

;;カーソル位置の保存
(when (require 'saveplace nil t)
	(setq-default save-place t)
	(setq save-place-file (concat user-emacs-directory ".emacs-plases"))
)

;; 鬼軍曹モード
;(when (require 'drill-instructor nil t)
;  (setq drill-instructor-global t))

;; diredを便利にする
(when (require 'dired-x nil t))

;; diredから"r"でファイル名をインライン編集する
(when (require 'wdired nil t)
	(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
)

;; grep結果を編集可能にする
(when (require 'wgrep nil t))

(global-unset-key "\C-t")
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
	;; keybind
	(global-set-key (kbd "C-M-c") 'mc/edit-lines)
	(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
	(when (require 'smartrep nil t)
		(declare-function smartrep-define-key "smartrep")
	(smartrep-define-key global-map "C-t"
		'(("C-t"      . 'mc/mark-next-like-this)
			("n"        . 'mc/mark-next-like-this)
			("p"        . 'mc/mark-previous-like-this)
			("m"        . 'mc/mark-more-like-this-extended)
			("u"        . 'mc/unmark-next-like-this)
			("U"        . 'mc/unmark-previous-like-this)
			("s"        . 'mc/skip-to-next-like-this)
			("S"        . 'mc/skip-to-previous-like-this)
			("a"        . 'mc/mark-all-like-this)
			("d"        . 'mc/mark-all-like-this-dwim)
			("i"        . 'my/mc/insert-numbers)
			("o"        . 'mc/sort-regions)
			("O"        . 'mc/reverse-regions)))
  )
)

;;Anything
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

;; Yasnippet
(when (require 'yasnippet nil t)
	(yas-global-mode 1)
	(custom-set-variables '(yas-trigger-key "TAB"))
	(add-hook 'term-mode-hook '(lambda () (yas-minor-mode -1) ))
	(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
	(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
	(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
)

;; multi-term
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

;;cua-mode                                                                                   
(cua-mode t)                                                                                 
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化

;; matlab-mode
(when (autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
	(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
	(setq auto-mode-alist (append '(("\\.m\\'" . matlab-mode)) auto-mode-alist))
	(add-hook 'matlab-mode-hook (lambda () (local-unset-key "\C-h")))
)

;; hexl-mode
(setq auto-mode-alist (append '(("\\.dll\\'" . hexl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.exe\\'" . hexl-mode)) auto-mode-alist))

;; mmm-mode
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
