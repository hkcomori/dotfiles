(require 'anything)
(define-key global-map (kbd "C-x C-b") 'anything-for-files)
(define-key global-map (kbd "M-y") 'anything-show-kill-ring)
(setq
 anything-idle-delay 0.1							;候補表示までの待ち時間
 anything-input-idle-delay 0.1				;再描画までの待ち時間
 anything-candidate-number-limit 100	;候補の最大数
 anything-enable-shortcuts 'alphabet	;候補選択ショートカットをアルファベットに
 anything-quick-update t							;候補が多い時に体感速度を早くする
 )
(when (and (executable-find "cmigemo") (require 'migemo nil t))
	(require 'anything-migemo nil t)
	)
