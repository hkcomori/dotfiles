;; dired関係の設定
(require 'dired-x)
(eval-after-load "dired-aux" '(require 'dired-async))

(setq dired-listing-switches "-ADGFLhl --group-directories-first --time-style=long-iso")

(cond ((eq system-type 'gnu/linux)
			 ;; 関連付けられたアプリを起動する(Linux)
			 (defun my-dired-related-open () 
				 "Type '[my-dired-related-open]': open the current line's file." 
				 (interactive) 
				 (if (eq major-mode 'dired-mode) 
						 (let ((flist (dired-get-marked-files)))
							 (dired-do-shell-command "exo-open" nil flist) 
							 (message "exo-open %s" flist)))))

			;; 関連付けられたアプリを起動する(Windows)
			((or (eq system-type 'windows-nt) (eq system-type 'cygwin))
			 (defun my-dired-related-open () 
				 "Type '[my-dired-related-open]': open the current line's file." 
				 (interactive) 
				 (if (eq major-mode 'dired-mode) 
						 (let ((fname (dired-get-filename))) 
							 (w32-shell-execute "open" fname) 
							 (message "open %s" fname)))
				 ))

			;; それ以外の場合はDiredで開く
			(t
			 (defun my-dired-related-open () 
				 "Type '[my-dired-related-open]': open the current line's file." 
				 (interactive) 
				 (dired-find-file)))
			 )

;; ファイルは関連付けで、ディレクトリは同じバッファで開く
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (cond ((file-directory-p file)
					 (dired-find-alternate-file))
					(t
					 (my-dired-related-open)))
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

;; すべてのファイル一括でマーク状態のトグルを行う
(defun dired-mark-all-files ()
  (interactive)
	(let ((flist (dired-get-marked-files)))
		(cond ((null flist)
					 (dired-toggle-marks))
					(t
					 (dired-unmark-all-marks)))
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

;; 更新日が今日であるファイルを色分けする
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

(when (require 'wdired nil t)
	(define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)	;diredから"r"でファイル名をインライン編集する
	(setq wdired-allow-to-change-permissions t)) ;パーミッションの編集を許可する

;; Diredの設定
(setq dired-dwim-target t)							;左右分割時に他方を転送先にする
(setq dired-recursive-copies  'always)	;常に再帰コピー
(setq dired-recursive-deletes 'always)	;常に再帰削除
(put 'dired-find-alternate-file 'disabled nil)

;; RET 標準の dired-find-file では dired バッファが複数作られるので
;; dired-find-alternate-file を代わりに使う
(define-key dired-mode-map (kbd "<RET>")			'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<mouse-1>")	'dired-open-in-accordance-with-situation)
(define-key dired-mode-map (kbd "e")					'dired-edit-in-accordance-with-situation)
(define-key dired-mode-map (kbd "<SPC>")			'dired-toggle-mark)
(define-key dired-mode-map (kbd "<DEL>")			'dired-up-directory)
(define-key dired-mode-map (kbd "k")					'dired-create-directory)
(define-key dired-mode-map (kbd "c")					'dired-do-copy)
(define-key dired-mode-map (kbd "d")					'dired-do-delete)
(define-key dired-mode-map (kbd "m")					'dired-do-rename)
(define-key dired-mode-map (kbd "*")					'dired-mark-all-files)
