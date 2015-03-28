;;-*- coding: utf-8; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 環境変数                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar oldemacs-p (<= emacs-major-version 22)) ;22 以下
(defvar emacs23-p (<= emacs-major-version 23))	;23 以下
(defvar emacs24-p (>= emacs-major-version 24))	;24 以上
(defvar darwin-p (eq system-type 'darwin))			;Mac OS X 用
(defvar nt-p
	(or (eq system-type 'windows-nt) (eq system-type 'cygwin)))			;Windows 用
(defvar linux-p (eq system-type 'gnu/linux))	;Linux 用

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; パス設定                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
	(let (path)
		(dolist (path paths paths)
			(let ((default-directory
							(expand-file-name (concat user-emacs-directory path))))
				(add-to-list 'load-path default-directory)
				(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
						(normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(when linux-p
	(add-to-load-path
	 "elpa"																;ELPAでインストールしたElisp
	 "elisp"															;手動でインストールしたElisp
	 )
	)
(when nt-p
	(add-to-load-path
	 "elpa"																;ELPAでインストールしたElisp
	 "elisp"															;手動でインストールしたElisp
	 "site-lisp"													;gnupack標準
	 )
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バッファ操作                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Close all buffers
(when (require 'cl nil t)
	(defun my/close-all-buffers ()
		(interactive)
		(loop for buffer being the buffers
					do (kill-buffer buffer))))

;; 変更されてないバッファを全部閉じる
;; http://qiita.com/amanoiverse/items/a3a605015d35c37efe2b
(defun my/close-all-unmodified-buffer ()
	(interactive)
	(let ((buffers (buffer-list)))
		(mapcar
		 #'(lambda (buf)
				 (if (and (not (buffer-modified-p buf))
									(not (string-match "^\\*\\(scratch\\|Messages\\|init log\\|terminal.*\\)\\*$" (buffer-name buf))))
						 (kill-buffer buf)))
		 buffers)
		))

;; カレントバッファのファイルをバイトコンパイル
;; http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-recompile-current-buffer ()
	"`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists."
	(interactive)
	(when (and (eq major-mode 'emacs-lisp-mode)
						 (file-exists-p (byte-compile-dest-file buffer-file-name)))
		(byte-compile-file buffer-file-name)))

;; カレントバッファのファイルをバイトコンパイル
;; http://ergoemacs.org/emacs/emacs_byte_compile.html
(defun byte-compile-current-buffer ()
	"`byte-compile' current buffer if it's emacs-lisp-mode."
	(interactive)
	(when (eq major-mode 'emacs-lisp-mode)
		(byte-compile-file buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイル操作                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun file-root-p (filename)
 "Return t if file FILENAME created by root."
 (eq 0 (nth 2 (file-attributes filename))))

(defun th-rename-tramp-buffer ()
 (when (file-remote-p (buffer-file-name))
   (rename-buffer
    (format "%s:%s"
            (file-remote-p (buffer-file-name) 'method)
            (buffer-name)))))

(add-hook 'find-file-hook 'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
 "Open FILENAME using tramp's sudo method if it's read-only."
 (if (and (file-root-p (ad-get-arg 0))
          (not (file-writable-p (ad-get-arg 0)))
          (y-or-n-p (concat "File "
                            (ad-get-arg 0)
                            " is read-only.  Open it as root? ")))
     (th-find-file-sudo (ad-get-arg 0))
   ad-do-it))

(defun th-find-file-sudo (file)
 "Opens FILE with root privileges."
 (interactive "F")
 (set-buffer (find-file (concat "/sudo::" file))))

;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
(defun dired-open-in-accordance-with-situation ()
  (interactive)
  (let ((file (dired-get-filename)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-word (arg)
	(interactive "p")
	(delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
	(interactive "p")
	(delete-word (- arg)))

(defun move-line-down ()
	(interactive)
	(let ((col (current-column)))
		(save-excursion
			(forward-line)
			(transpose-lines 1))
		(forward-line)
		(move-to-column col)))

(defun move-line-up ()
	(interactive)
	(let ((col (current-column)))
		(save-excursion
			(forward-line)
			(transpose-lines -1))
		(when nt-p (forward-line -1))
		(move-to-column col)))

(defun mark-current-word (&optional arg allow-extend)
	"Put point at beginning of current word, set mark at end."
	(interactive "p")
	(setq arg (if arg arg 1))
	(if (and allow-extend
					 (or (and (eq last-command this-command) (mark t))
							 (region-active-p)))
			(set-mark
			 (save-excursion
				 (when (< (mark) (point))
					 (setq arg (- arg)))
				 (goto-char (mark))
				 (forward-word arg)
				 (point)))
		(let ((wbounds (bounds-of-thing-at-point 'word)))
			(unless (consp wbounds)
				(error "No word at point"))
			(if (>= arg 0)
					(goto-char (car wbounds))
				(goto-char (cdr wbounds)))
			(push-mark (save-excursion
									 (forward-word arg)
									 (point)))
			(activate-mark))))

(defun mark-current-sexp (&optional arg allow-extend)
	"Put point at beginning of current word, set mark at end."
	(interactive "p")
	(setq arg (if arg arg 1))
	(if (and allow-extend
					 (or (and (eq last-command this-command) (mark t))
							 (region-active-p)))
			(set-mark
			 (save-excursion
				 (when (< (mark) (point))
					 (setq arg (- arg)))
				 (goto-char (mark))
				 (forward-sexp arg)
				 (point)))
		(let ((wbounds (bounds-of-thing-at-point 'sexp)))
			(unless (consp wbounds)
				(error "No sexp at point"))
			(if (>= arg 0)
					(goto-char (car wbounds))
				(goto-char (cdr wbounds)))
			(push-mark (save-excursion
									 (forward-sexp arg)
									 (point)))
			(activate-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; マウス操作                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun mouse/mark-current-word (click)
	(interactive "e")
	(mouse-set-point click)
	(mark-current-word))

(defun mouse/mark-current-sexp (click)
	(interactive "e")
	(mouse-set-point click)
	(mark-current-sexp))
