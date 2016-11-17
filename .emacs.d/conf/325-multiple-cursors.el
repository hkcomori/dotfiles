;;; Code:
(require 'multiple-cursors)
;; insert specific serial number
(defvar my/mc/insert-numbers-hist nil)
(defvar my/mc/insert-numbers-inc 1)
(defvar my/mc/insert-numbers-pad "%01d")
(defun my/mc/insert-numbers (start inc pad)
	"Insert increasing numbers for each cursor specifically."
	(interactive
	 (list (read-number "Start from: " 0)
				 (read-number "Increment by: " 1)
				 (read-string "Padding (%01d): " nil my/mc/insert-numbers-hist "%01d")))
	(setq mc--insert-numbers-number start)
	(setq my/mc/insert-numbers-inc inc)
	(setq my/mc/insert-numbers-pad pad)
	(mc/for-each-cursor-ordered
	 (mc/execute-command-for-fake-cursor
	  'my/mc--insert-number-and-increase
	  cursor)))
(defun my/mc--insert-number-and-increase ()
	(interactive)
	(insert (format my/mc/insert-numbers-pad mc--insert-numbers-number))
	(setq mc--insert-numbers-number (+ mc--insert-numbers-number my/mc/insert-numbers-inc)))
(defun mc/mark-next-like-this-and-cycle-forward ()
	(interactive)
	(mc/mark-next-like-this 1)
	(mc/cycle-forward))
(defun mc/mark-previous-like-this-and-cycle-backward ()
	(interactive)
	(mc/mark-previous-like-this 1)
	(mc/cycle-backward))

;; multiple-cursors使用時に一回のみ実行する関数の設定
(add-to-list 'mc--default-cmds-to-run-once 'my/mc/insert-numbers)
(add-to-list 'mc--default-cmds-to-run-once 'my/mc--insert-number-and-increase)
(add-to-list 'mc--default-cmds-to-run-once 'mc/mark-next-like-this-and-cycle-forward)
(add-to-list 'mc--default-cmds-to-run-once 'mc/mark-previous-like-this-and-cycle-backward)
(add-to-list 'mc--default-cmds-to-run-for-all 'point-undo)
(add-to-list 'mc--default-cmds-to-run-for-all 'point-redo)
(add-to-list 'mc--default-cmds-to-run-for-all 'indent-for-tab-command)
(add-to-list 'mc--default-cmds-to-run-for-all 'comment-dwim)
(add-to-list 'mc--default-cmds-to-run-for-all 'delete-word)
(add-to-list 'mc--default-cmds-to-run-for-all 'cua-paste)
(add-to-list 'mc--default-cmds-to-run-for-all 'beginning-of-defun)
(add-to-list 'mc--default-cmds-to-run-for-all 'visual-basic-beginning-of-defun)

(define-key global-map (kbd "C-M-c") 'mc/edit-lines)
(define-key global-map (kbd "C-M-r") 'mc/mark-all-in-region)
(when (require 'smartrep nil t)
	;; smartrepによるコマンド実行中はキー入力をエコーしない
	;; http://shakenbu.org/yanagi/d/?date=20140105
	(defadvice smartrep-map-internal (around smartrep-silence-echo-keystrokes activate)
		(let ((echo-keystrokes 0)) ad-do-it))
	(declare-function smartrep-define-key "smartrep")
	(global-unset-key "\C-t")
	(smartrep-define-key global-map "C-t"
		'(("C-t" . 'mc/mark-next-like-this-and-cycle-forward)
			("n"   . 'mc/mark-next-like-this-and-cycle-forward)
			("p"   . 'mc/mark-previous-like-this-and-cycle-backward)
			("C-v" . 'mc/cycle-forward)
			("M-v" . 'mc/cycle-backward)
			("m"   . 'mc/mark-more-like-this-extended)
			("u"   . 'mc/unmark-next-like-this)
			("U"   . 'mc/unmark-previous-like-this)
			("s"   . 'mc/skip-to-next-like-this)
			("S"   . 'mc/skip-to-previous-like-this)
			("a"   . 'mc/mark-all-like-this)
			("d"   . 'mc/mark-all-like-this-dwim)
			("i"   . 'my/mc/insert-numbers)
			("o"   . 'mc/sort-regions)
			("O"   . 'mc/reverse-regions)))
	)

(define-key mc/keymap [return] 'newline-and-indent)
(define-key mc/keymap [zenkaku-hankaku] 'toggle-input-method)
(define-key mc/keymap [kanji] 'toggle-input-method)

(when (require 'phi-search-migemo nil t)
	(define-key phi-search-default-map (kbd "M-m") 'phi-search-migemo-toggle)
	(define-key mc/keymap (kbd "C-s") 'phi-search-migemo)
	(define-key mc/keymap (kbd "C-r") 'phi-search-migemo-backward)
	)

(setq smartrep-mode-line-active-bg my-color-darkgreen)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 325-multiple-cursors.el ends here
