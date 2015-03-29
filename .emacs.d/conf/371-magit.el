(require 'magit)
;; キーバインド
(define-key mode-specific-map "m" 'magit-status)
;; GUI設定
(set-face-background 'magit-item-highlight (face-attribute 'default :background))

;; 文字色変更
(set-face-foreground 'magit-diff-add						"#00CC00") ; 追加行
(set-face-foreground 'magit-diff-del						"#CC0000") ; 削除行
(set-face-foreground 'magit-diff-file-header		"#0000FF") ; Diffのファイル名部分
(set-face-foreground 'magit-diff-hunk-header		"#00BFFF") ; Diffのhunk部分
(set-face-foreground 'magit-section-title				"#FF00FF") ; 見出し部分

;; 背景色変更
(set-face-background 'magit-item-highlight			(face-attribute 'default :background)) ; 選択行
(set-face-background 'magit-diff-add						(face-attribute 'default :background)) ; 追加行
(set-face-background 'magit-diff-del						(face-attribute 'default :background)) ; 削除行
(set-face-background 'magit-diff-file-header		(face-attribute 'default :background)) ; Diffのファイル名部分
(set-face-background 'magit-diff-hunk-header		(face-attribute 'default :background)) ; Diffのhunk部分
(set-face-background 'magit-section-title				(face-attribute 'default :background)) ; 見出し部分
