;;-*- coding: utf-8; -*-

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

(define-key global-map (kbd "C-x C-c") 'my/close-all-unmodified-buffer)	;バッファをすべて閉じる

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

(add-hook 'after-save-hook 'byte-recompile-current-buffer)

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

(define-key global-map (kbd "M-N")							'move-line-down)				;行を一行下へ
(define-key global-map (kbd "M-P")							'move-line-up)					;行を一行上へ

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
