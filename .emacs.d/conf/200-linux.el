;;-*- coding: utf-8; -*-
;;; Code:
(when (eq system-type 'gnu/linux)
  ;; setting for japanese
  (set-default-coding-systems 'utf-8-unix)				;デフォルトの文字コード
  (prefer-coding-system 'utf-8-unix)							;テキストファイル／新規バッファの文字コード
  (set-file-name-coding-system 'utf-8-unix)			;ファイル名の文字コード
  (set-keyboard-coding-system 'utf-8-unix)			;キーボード入力の文字コード

	(add-hook 'minibuffer-setup-hook	'fcitx-disable)
	(defun fcitx-disable ()
		"Turn off fcitx ime."
		(interactive)
		(let ((shell-command-no-message t))
			(shell-command "fcitx-remote -c")))

  ;; YaTeX-mode
  (setq auto-mode-alist  (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
  (autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
  ;;(setq load-path (cons "~/src/yatex" load-path))
  (setq tex-command "platex")
  ;;(setq dvi2-command "")
  (setq dviprint-command-format "dvipdfmx %s ")
  (add-hook 'yatex-mode-hook 'turn-on-reftex)

  ;; X-window のクリップボードと emacs のクリップボードの同期をとる
  (setq x-select-enable-clipboard t)

  ;;====================================
  ;; Keybind
  ;;====================================
  (define-key global-map "\M-[1;2A" 'windmove-up)
  (define-key global-map "\M-[1;2B" 'windmove-down)
  (define-key global-map "\M-[1;2C" 'windmove-right)
  (define-key global-map "\M-[1;2D" 'windmove-left)
)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 200-linux.el ends here
