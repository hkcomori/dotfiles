;;-*- coding: utf-8; -*-
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Binary                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.\\(exe\\|dll\\)$" . hexl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Lisp                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 'add-hook/emacs-lisp-mode-common-hook)
(defun add-hook/emacs-lisp-mode-common-hook ()
	(setq tab-width 2)
	(setq moccur-grep-default-mask "\\.el$")
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'c-mode-common-hook 'add-hook/c-mode-common-hook)
(defun add-hook/c-mode-common-hook ()
	(c-set-style "CC-MODE")			;;自動インデントのスタイルを変更
	(enable-reindent)
	(setq moccur-grep-default-mask "\\.\\(c\\|cs\\|cpp\\|h\\)$")
	)

;; Syntaxハイライト設定
(defface font-lock-c-di-face '((t ((:foreground my-color-blue)))) nil)
(defface font-lock-c-ei-face '((t ((:foreground my-color-red)))) nil)
(font-lock-add-keywords
 'c-mode '(("\\<m?i_[a-zA-Z0-9_]+\\>"						. 'font-lock-variable-name-face)
					 ("\\<IntcrlDisableAllInterrupt\\>()"	. 'font-lock-c-di-face)
					 ("\\<IntcrlEnableAllInterrupt\\>()"	. 'font-lock-c-ei-face)
					 ("\\<MYSTATIC\\>"										. 'font-lock-keyword-face)
					 ("\\<\\(ST\\|EM\\)_[a-zA-Z0-9_]+\\>"	. 'font-lock-type-face)
					 ("\\<[A-Z][A-Z0-9_]+[A-Z0-9]\\>"			. 'font-lock-constant-face)
					 ("\\<\\(VALID\\|INVALID\\)\\>"				. 'font-lock-constant-face)))

(defun c-func-list ()
	(interactive)
	(occur-by-moccur "^[_a-zA-Z][^()+-=;:]*\\s-+[_a-zA-Z][_a-zA-Z0-9]*(.*$" " ")
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell Script                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'sh-mode-hook 'enable-reindent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PowerShell                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))
(autoload 'powershell-mode "powershell" "A editing mode for Microsoft PowerShell." t)
(autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 'enable-reindent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ruby                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'ruby-mode-hook 'enable-reindent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JavaScript                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML 中の Java Script
;; (autoload 'javascript-mode "javascript" "JavaScript mode" t)
;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe nil t)
;; (setq mmm-submode-decoration-level 2)
;; ;; js in html
;; (mmm-add-classes
;;  '((js-in-html
;; 		:submode javascript-mode
;; 		:front "<script[^>]*>\n<!--\n"
;; 		:back  "// ?-->\n</script>")))
;; (mmm-add-mode-ext-class nil "\\.s?html?\\(\\..+\\)?$" 'js-in-html)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Basic                                                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" . visual-basic-mode))
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(add-hook 'visual-basic-mode-hook '(lambda () (setq indent-tabs-mode 'nil)))
(setq visual-basic-mode-indent 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MATLAB                                                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.m$" . matlab-mode))
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(add-hook 'matlab-mode-hook (lambda () (local-unset-key "\C-h")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arduino                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.\\(pde\\|ino\\)$" . arduino-mode))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 103-filetype.el ends here
