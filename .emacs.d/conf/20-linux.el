;;-*- coding: utf-8; -*-
(when linux-p
  ;; setting for japanese
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8-unix)
  (setq file-name-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)        ; 文字コード認識優先順位１
  (prefer-coding-system 'cp932-dos)    ; 文字コード認識優先順位２
  (set-default-coding-systems 'utf-8)

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
  ;; Extension
  ;;====================================
;;; sr-speedbar.el
  ;;(require 'sr-speedbar)
  ;;(sr-speedbar-open)


  ;;====================================
  ;; Keybind
  ;;====================================
  (global-set-key "\M-[1;2A" 'windmove-up)
  (global-set-key "\M-[1;2B" 'windmove-down)
  (global-set-key "\M-[1;2C" 'windmove-right)
  (global-set-key "\M-[1;2D" 'windmove-left)
  ;;(global-set-key "\C-x\C-c" 'kill-buffer)

  ;;====================================
  ;; Edit
  ;;====================================

  ;;====================================
  ;; Misc
  ;;====================================
)
