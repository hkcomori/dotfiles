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
	(define-key global-map (kbd "M-/") 'point-undo)
	(define-key global-map (kbd "M-\\") 'point-redo)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;矩形選択を有効化                                                                                   
(cua-mode t)                                                                                 
(setq cua-enable-cua-keys nil) ; そのままだと C-x が切り取りになってしまったりするので無効化

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 履歴                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;カーソル位置の保存
(when (require 'saveplace nil t)
	(setq-default save-place t)
	(setq save-place-file (concat user-emacs-directory ".emacs-places"))
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

(when (require 'ediff nil t)
	(setq ediff-window-setup-function 'ediff-setup-windows-plain)	; コントロール用のバッファを同一フレーム内に表示
	(setq ediff-split-window-function 'split-window-horizontally)	; diffのバッファを上下ではなく左右に並べる
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; その他                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 関数一覧
(when (require 'summarye nil t)
	(define-key mode-specific-map "l" 'se/make-summary-buffer)
	)

;; howm
(when (require 'howm nil t)
	(setq howm-directory (concat user-emacs-directory "howm"))
	(setq howm-menu-lang 'ja)
	(setq howm-file-name-format "%Y-%m.howm")
	(add-to-list 'load-path "elisp/howm")
	(define-key global-map "\C-c,," 'howm-menu)
	)

(when (require 'flycheck nil t)
	(add-hook 'after-init-hook #'global-flycheck-mode)
	)
