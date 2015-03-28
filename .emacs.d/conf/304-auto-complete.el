(require 'auto-complete)
(defun my/ac-mode-enable ()
	(auto-complete-mode -1)
	(auto-complete-mode t))
(defun my/ac-mode-disable ()
	(auto-complete-mode -1))
(global-auto-complete-mode t)			;自動補完を常に有効
(setq ac-auto-show-menu 0.3)			;候補が出るまでの時間 (default: 0.8)
