;;; Commentary:
;; dired関係の設定

;;; Code:

(require 'dired-x)
(require 'wdired)

(setq wdired-allow-to-change-permissions t) ;パーミッションの編集を許可する
(put 'dired-find-alternate-file 'disabled nil)

(setq dired-listing-switches "-DGFLhl --group-directories-first --time-style=long-iso")

(define-minor-mode my-dired-display-all-mode
  ""
  :init-value nil
  (if my-dired-display-all-mode
      (setq dired-actual-switches
            (concat "-A "
                    dired-actual-switches))
    (setq dired-actual-switches
          (replace-regexp-in-string "-A " "" dired-actual-switches)))
  (when (eq major-mode 'dired-mode)
    (revert-buffer)))

;; ファイルは別バッファで、ディレクトリは同じバッファで開く
(defun dired-edit-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((file-directory-p file)
					 (dired-find-alternate-file))
					(t
					 (dired-find-file)))
		))

;; すべてのファイル一括でマーク状態のトグルを行う
(defun dired-mark-all-files ()
  (interactive)
	(let ((fn-list (dired-get-marked-files)))
		(cond ((null fn-list)
					 (dired-toggle-marks))
					(t
					 (dired-toggle-marks)))
		))

;; フォルダ移動でバッファを新しく作らない
(defadvice dired-up-directory
	(before kill-up-dired-buffer activate)
	(setq my-dired-before-buffer (current-buffer)))
(defadvice dired-up-directory
	(after kill-up-dired-buffer-after activate)
	(if (eq major-mode 'dired-mode)
			(kill-buffer my-dired-before-buffer)))

;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "<RET>")			'dired-edit-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<mouse-1>")	'ignore)													;ファイル行のクリック動作
(define-key dired-mode-map (kbd "<mouse-2>")	'ignore)													;ファイル名のクリック動作
(define-key dired-mode-map (kbd "e")					'dired-edit-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<DEL>")			'dired-up-directory)							;上のディレクトリへ移動
(define-key dired-mode-map (kbd "r")					'wdired-change-to-wdired-mode)		;ファイル名をインライン編集
(define-key dired-mode-map (kbd "h")					'my-dired-display-all-mode)				;隠しファイルを表示/非表示
(define-key dired-mode-map (kbd ".")					'my-dired-display-all-mode)				;隠しファイルを表示/非表示
(define-key dired-mode-map (kbd "*")					'dired-mark-all-files)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 310-dired.el ends here
