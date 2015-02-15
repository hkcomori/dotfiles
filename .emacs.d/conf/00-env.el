;;-*- coding: utf-8; -*-
(defvar oldemacs-p (<= emacs-major-version 22)) ;22 以下
(defvar emacs23-p (<= emacs-major-version 23))	;23 以下
(defvar emacs24-p (>= emacs-major-version 24))	;24 以上
(defvar darwin-p (eq system-type 'darwin))			;Mac OS X 用
(defvar nt-p
	(or (eq system-type 'windows-nt) (eq system-type 'cygwin)))			;Windows 用
(defvar linux-p (eq system-type 'gnu/linux))	;Linux 用

;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
	(let (path)
		(dolist (path paths paths)
			(let ((default-directory
							(expand-file-name (concat user-emacs-directory path))))
				(add-to-list 'load-path default-directory)
				(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
						(normal-top-level-add-subdirs-to-load-path))))))

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
		(previous-line)
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

(defun mouse/mark-current-word (click)
	(interactive "e")
	(mouse-set-point click)
	(mark-current-word))

(defun mouse/mark-current-sexp (click)
	(interactive "e")
	(mouse-set-point click)
	(mark-current-sexp))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(when linux-p
	(add-to-load-path
	 "elpa"								;ELPAでインストールしたElisp
	 "elisp"							;手動でインストールしたElisp
	 )
	)
(when nt-p
	(add-to-load-path
	 "elpa"								;ELPAでインストールしたElisp
	 "elisp"								;手動でインストールしたElisp
	 "site-lisp"							;gnupack標準
	 )
	)
