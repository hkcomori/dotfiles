(require 'magit)
;; キーバインド
(define-key mode-specific-map "m" 'magit-status)
;; GUI設定
(set-face-background 'magit-item-highlight (face-attribute 'default :background))
