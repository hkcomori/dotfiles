;;; Code:
(require 'auto-complete)
(require 'auto-complete-config)
(defun my/ac-mode-enable ()
	(auto-complete-mode -1)
	(auto-complete-mode t))
(defun my/ac-mode-disable ()
	(auto-complete-mode -1))
;;(global-auto-complete-mode t)			;自動補完を常に有効
(ac-config-default)
(setq ac-use-menu-map t)
(setq ac-use-fuzzy t)
(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.1)			;候補が出るまでの時間 (default: 0.8)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 302-auto-complete.el ends here
