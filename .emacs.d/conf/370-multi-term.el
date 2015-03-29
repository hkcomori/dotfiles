;;; Code:

(require 'multi-term)
(setq multi-term-program shell-file-name)
(defun toggle-term-view () (interactive)
	(cond ((eq major-mode 'term-mode)
				 (fundamental-mode)
				 (view-mode-enable)
				 (local-set-key (kbd "C-c C-c") 'toggle-term-view)
				 (setq multi-term-cursor-point (point)))
				((eq major-mode 'fundamental-mode)
				 (view-mode-disable)
				 (goto-char multi-term-cursor-point)
				 (multi-term-internal))))
(add-hook 'term-mode-hook
					(lambda ()
						(define-key term-raw-map (kbd "C-y") 'term-paste)
						(define-key term-raw-map (kbd "C-h") 'term-send-backspace)
						(define-key term-raw-map (kbd "M-d") 'term-send-forward-kill-word)
						(define-key term-raw-map (kbd "M-h") 'term-send-backward-kill-word)
						(define-key term-raw-map (kbd "C-r") 'term-send-reverse-search-history)
						(define-key term-raw-map (kbd "C-s") 'term-send-reverse-search-history)
						(define-key term-raw-map (kbd "M-<backspace>") 'term-send-backward-kill-word)
						(define-key term-raw-map (kbd "M-DEL") 'term-send-backward-kill-word)
						(define-key term-raw-map (kbd "C-v") nil)
						(define-key term-raw-map (kbd "ESC ESC") 'term-send-raw)
						(define-key term-raw-map (kbd "C-q") 'toggle-term-view)
						(define-key term-raw-map (kbd "C-DEL") 'term-send-forward-kill-word)
						(define-key term-raw-map (kbd "C-<backspace>") 'term-send-backward-kill-word)
						(define-key term-raw-map (kbd "M-<right>") 'term-send-forward-word)
						(define-key term-raw-map (kbd "C-<right>") 'term-send-forward-word)
						(define-key term-raw-map (kbd "M-<left>") 'term-send-backward-word)
						(define-key term-raw-map (kbd "C-<left>") 'term-send-backward-word)
						))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 370-multi-term.el ends here