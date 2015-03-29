;;; Code:
(setq-default bm-buffer-persistence nil)
(setq bm-restore-repository-on-load t)
(require 'bm)
(add-hook 'find-file-hook 'bm-buffer-restore)
(add-hook 'kill-buffer-hook 'bm-buffer-save)
(add-hook 'after-save-hook 'bm-buffer-save)
(add-hook 'after-revert-hook 'bm-buffer-restore)
(add-hook 'vc-before-checkin-hook 'bm-buffer-save)
(add-hook 'kill-emacs-hook '(lambda nil
                              (bm-buffer-save-all)
                              (bm-repository-save)))
(define-key global-map (kbd "M-SPC") 'bm-toggle)
(define-key global-map (kbd "M-[") 'bm-previous)
(define-key global-map (kbd "M-]") 'bm-next)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 321-bm.el ends here
