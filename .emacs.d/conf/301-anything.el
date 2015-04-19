;;; Code:
(require 'anything)
(require 'anything-config)

(define-key global-map (kbd "C-x C-b") 'anything-for-files)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)

(define-key global-map (kbd "M-X") 'anything-execute-emacs-commands)
(defun anything-execute-emacs-commands ()
	"Execute emacs commands in anything"
	(interactive)
	(anything '(anything-c-source-emacs-commands)))

(setq
 anything-idle-delay 0.1							;候補表示までの待ち時間
 anything-input-idle-delay 0.2				;再描画までの待ち時間
 anything-candidate-number-limit 100	;候補の最大数
 anything-enable-shortcuts 'alphabet	;候補選択ショートカットをアルファベットに
 anything-quick-update t							;候補が多い時に体感速度を早くする
 )

;; 入力終了までバッファの更新を抑制
(defadvice anything-check-minibuffer-input (around sit-for activate)
	(if (sit-for anything-idle-delay t)
			ad-do-it))

(when (and (executable-find "cmigemo") (require 'migemo nil t))
	(require 'anything-migemo nil t)
	)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 301-anything.el ends here
