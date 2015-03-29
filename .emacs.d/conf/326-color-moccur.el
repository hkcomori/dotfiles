;;; Code:
(require 'color-moccur)
(require 'moccur-edit)

(setq moccur-split-word t)							;スペース区切りでAND検索

;; moccur-edit-finish-editと同時にファイルを保存する
(defadvice moccur-edit-change-file
	(after save-after-moccur-edit-buffer activate)
	(save-buffer))

(define-key global-map (kbd "M-o")	'occur-by-moccur)
(define-key global-map (kbd "M-g")	'moccur-grep-find)

(when (and (executable-find "cmigemo")
					 (require 'migemo nil t))
	(setq moccur-use-migemo t))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 326-color-moccur.el ends here
