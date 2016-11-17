;;; Code:
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - migemo                                               ;;;
;;;   https://github.com/emacs-jp/migemo                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
(require 'migemo)
(when (executable-find "cmigemo")
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (when (eq system-type 'cygwin)		(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
  (when (eq system-type 'gnu/linux)	(setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))
  (defvar migemo-user-dictionary nil)
  (defvar migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init)
)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 321-migemo.el ends here
