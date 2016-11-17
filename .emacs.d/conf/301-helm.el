;;; Code:
(require 'helm)
(require 'helm-config)
(require 'helm-migemo)
(require 'helm-mode)

(define-key global-map (kbd "C-x C-r")	'helm-resume)
(define-key global-map (kbd "C-x C-f")	'helm-find-files)
(define-key global-map (kbd "C-x C-b")	'helm-for-files)
(define-key global-map (kbd "C-x b")		'helm-buffers-list)
(define-key global-map (kbd "M-y")			'helm-show-kill-ring)

(define-key helm-map [zenkaku-hankaku]	'toggle-input-method)

;; 既存のコマンドをHelmインタフェースに置き換える
(helm-mode 1)

;;; 自動補完を無効
(custom-set-variables '(helm-ff-auto-update-initial-value nil))
;;; helm-mode で無効にしたいコマンド
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(find-file-at-point . nil))
(add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
(add-to-list 'helm-completing-read-handlers-alist '(helm-c-yas-complete . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . nil))
(add-to-list 'helm-completing-read-handlers-alist '(dired-create-directory . nil))

;;; 一度に表示する最大候補数を増やす
(setq helm-candidate-number-limit 99999)

(set-face-foreground 'helm-selection	(face-attribute 'default :background))
(set-face-background 'helm-selection	(face-attribute 'default :foreground))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 301-helm.el ends here
