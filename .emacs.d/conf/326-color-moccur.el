;;; Code:
(require 'color-moccur)
(require 'moccur-edit)

(setq moccur-split-word t)							;スペース区切りでAND検索

(define-key global-map (kbd "M-o")	'occur-by-moccur)
(define-key global-map (kbd "M-g")	'moccur-grep-find)

(setq moccur-use-migemo t)

(set-face-attribute 'moccur-face nil
										:foreground "unspecified"
										:background my-color-darkgray)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 326-color-moccur.el ends here
