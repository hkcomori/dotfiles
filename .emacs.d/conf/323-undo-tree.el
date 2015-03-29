;;; Code:
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree"))))
(setq undo-limit 600000)
(setq undo-strong-limit 900000)
(define-key global-map (kbd "C-/") 'undo-tree-undo)
(define-key global-map (kbd "C-\\") 'undo-tree-redo)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 323-undo-tree.el ends here
