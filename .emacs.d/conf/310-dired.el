;;; Commentary:
;; dired関係の設定

;;; Code:

(require 'dired-x)
(require 'wdired)
(eval-after-load "dired-aux" '(require 'dired-async))

(setq dired-dwim-target t)							;左右分割時に他方を転送先にする
(setq dired-recursive-copies  'always)	;常に再帰コピー
(setq dired-recursive-deletes 'always)	;常に再帰削除
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

(cond ((eq system-type 'gnu/linux)
			 (defun dired-open-dwim () 
				 "Open each of the marked files, or the file under the point with default application." 
				 (interactive) 
				 (if (eq major-mode 'dired-mode) 
						 (let* ((fn-list (dired-get-marked-files)))
							 (exec-command "exo-open" fn-list)))
				 ))

			((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
			 (defun dired-open-dwim () 
				 "Open file under the cursor." 
				 (interactive) 
				 (if (eq major-mode 'dired-mode) 
						 (let ((fname (dired-get-filename))) 
							 (w32-shell-execute "open" fname) 
							 (message "open %s" fname)))
				 ))

			(t
			 (defun dired-open-dwim () 
				 "." 
				 (interactive) 
				 (if (eq major-mode 'dired-mode) 
						 (let* ((fn-list (dired-get-marked-files)))
							 (dired-x-find-file-other-window fn-list)))
			 ))
			)

;; ファイルは関連付けで、ディレクトリは同じバッファで開く
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((file-directory-p file)
					 (dired-find-alternate-file))
					(t
					 (dired-open-dwim)))
		))

;; ファイルは別バッファで、ディレクトリは同じバッファで開く
(defun dired-edit-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((file-directory-p file)
					 (dired-find-alternate-file))
					(t
					 (dired-find-file)))
		))

(define-key dired-mode-map (kbd "u")					'dired-unpack-files)
(defun dired-unpack-files ()
	"Unpack each of the marked files, or the file under the point."
	(interactive)
	(let* ((fn-list (dired-get-marked-files)))
		(async-shell-command (concat "unar " (concat-string-list fn-list)))))

(define-key dired-mode-map (kbd "p")					'dired-pack-files)
(defun dired-pack-files (fn-dest)
	"Archive each of the marked files, or the file under the point."
	(interactive
	 (let* ((fn-list (dired-get-marked-files))
					(default-fn (concat (file-name-sans-extension (file-name-nondirectory (car fn-list))) ".zip")))
		 (list (read-file-name "Archive file name: " nil nil nil default-fn))))
	(let* ((fn-list (dired-get-marked-files)))
		(async-shell-command (concat "zip -r \"" (expand-file-name fn-dest) "\" " (concat-string-list fn-list)))
		))

;; diredでマークをつけたファイルを開く
(define-key dired-mode-map (kbd "F") 'my-dired-find-marked-files)
(defun my-dired-find-marked-files (&optional arg)
	"Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
	(interactive "P")
	(let* ((fn-list (dired-get-marked-files nil arg)))
		(mapc 'find-file fn-list)))

;; diredでマークをつけたファイルをviewモードで開く
(define-key dired-mode-map (kbd "V") 'my-dired-view-marked-files)
(defun my-dired-view-marked-files (&optional arg)
	"Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
	(interactive "P")
	(let* ((fn-list (dired-get-marked-files nil arg)))
		(mapc 'view-file fn-list)))

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

;; マーク操作を同じ関数でトグル
(defun dired-toggle-mark (arg)
	"Toggle the current (or next ARG) files."
	;; S.Namba Sat Aug 10 12:20:36 1996
	(interactive "P")
	(let ((dired-marker-char
				 (if (save-excursion (beginning-of-line)
														 (looking-at " "))
						 dired-marker-char ?\040)))
		(dired-mark arg)))

(defun dired-toggle-mark-and-prev (arg)
	"Toggle the current (or next ARG) files."
	;; S.Namba Sat Aug 10 12:20:36 1996
	(interactive "P")
	(dired-toggle-mark arg)
	(dired-previous-line 2))

;; 本日更新のファイルの日付を色分けする
(defface dired-todays-face '((t (:foreground "blue"))) nil)
(defvar dired-todays-face 'dired-todays-face)
(defconst month-name-alist
	'(("1"  . "Jan") ("2"  . "Feb") ("3"  . "Mar") ("4"  . "Apr")
		("5"  . "May") ("6"  . "Jun") ("7"  . "Jul") ("8"  . "Aug")
		("9"  . "Sep") ("10" . "Oct") ("11" . "Nov") ("12" . "Dec")))
(defun dired-today-search (arg)
	"Fontlock search function for dired."
	(search-forward-regexp
	 (let ((month-name
					(cdr (assoc (format-time-string "%b") month-name-alist))))
		 (if month-name
				 (format
					(format-time-string
					 "\\(%Y-%m-%d\\|%b %e\\|%%s %e\\) [0-9]....") month-name)
			 (format-time-string
				"\\(%Y-%m-%d\\|%b %e\\) [0-9]....")))
	 arg t))
(eval-after-load "dired"
	'(font-lock-add-keywords
		'dired-mode
		(list '(dired-today-search . dired-todays-face))))


;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "<RET>")			'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<mouse-1>")	'ignore)													;ファイル行のクリック動作
(define-key dired-mode-map (kbd "<mouse-2>")	'ignore)													;ファイル名のクリック動作
(define-key dired-mode-map (kbd "e")					'dired-edit-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<SPC>")			'dired-toggle-mark)								;マークをオン/オフする
(define-key dired-mode-map (kbd "<S-SPC>")		'dired-toggle-mark-and-prev)			;マークをオン/オフする
(define-key dired-mode-map (kbd "<DEL>")			'dired-up-directory)							;上のディレクトリへ移動
(define-key dired-mode-map (kbd "k")					'dired-create-directory)					;ディレクトリを作成
(define-key dired-mode-map (kbd "c")					'dired-do-copy)										;ファイルをコピー
(define-key dired-mode-map (kbd "d")					'dired-do-delete)									;ファイルを削除
(define-key dired-mode-map (kbd "r")					'wdired-change-to-wdired-mode)		;ファイル名をインライン編集
(define-key dired-mode-map (kbd "m")					'dired-do-rename)									;ファイルを移動
(define-key dired-mode-map (kbd "h")					'my-dired-display-all-mode)				;ファイルを移動
(define-key dired-mode-map (kbd ".")					'my-dired-display-all-mode)				;ファイルを移動
(define-key dired-mode-map (kbd "*")					'dired-mark-all-files)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 310-dired.el ends here
