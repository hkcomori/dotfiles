;;-*- coding: utf-8; -*-
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elisp拡張                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun concat-string-list (list) 
	"Return a string which is a concatenation of all elements of the list separated by spaces" 
	(mapconcat '(lambda (obj) (format "\"%s\"" obj)) list " "))

(defun enable-reindent ()
	(local-set-key (kbd "C-m") 'reindent-then-newline-and-indent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; バッファ操作                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 変更されてないバッファを全部閉じる
;; http://qiita.com/amanoiverse/items/a3a605015d35c37efe2b
(define-key global-map (kbd "C-x C-c") 'my/close-all-unmodified-buffer)
(defun my/close-all-unmodified-buffer ()
	(interactive)
	(if (not (string= "No server editing buffers exist" (server-edit)))
			;; emacsclient経由のバッファが残っている場合は、それをすべて閉じる
			(while (not (string= "No server editing buffers exist" (server-edit))))
		;; 残っていない場合は、変更されていないバッファをすべて閉じる
		(let ((buffers (buffer-list)))
			(mapcar
			 #'(lambda (buf)
					 (if (and (not (buffer-modified-p buf))
										(not (string-match "^\\*\\(scratch\\|Messages\\|init log\\|terminal.*\\)\\*$" (buffer-name buf))))
							 (kill-buffer buf)))
			 buffers)
			)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイル操作                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun exec-command (command &optional args)
	"Run external COMMAND at background."
	(let* ((process-connection-type nil))
		(apply 'start-process command nil command args))
	)

(defvar shell-command-no-message nil)
(defadvice shell-command
	(after no-message activate)
	"suppress the output from `message' to minibuffer"
	(if shell-command-no-message (message "")))

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

(defun file-name-insert (arg)
  (interactive "p")
  (insert (file-name-nondirectory buffer-file-name)))

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
		(when (eq system-type 'cygwin) (forward-line -1))
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

;; Local Variables:
;; coding: utf-8
;; End:

;;; 003-func.el ends here
