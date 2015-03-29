;;-*- coding: utf-8; -*-
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ファイルタイプ                                                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Arduino
(setq auto-mode-alist (cons '("\\.\\(pde\\|ino\\)$" . arduino-mode) auto-mode-alist))
(autoload 'arduino-mode "arduino-mode" "Arduino editing mode." t)

;; MATLAB
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(setq auto-mode-alist (append '(("\\.m\\'" . matlab-mode)) auto-mode-alist))
(add-hook 'matlab-mode-hook (lambda () (local-unset-key "\C-h")))

;; バイナリ
(setq auto-mode-alist (append '(("\\.dll\\'" . hexl-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.exe\\'" . hexl-mode)) auto-mode-alist))

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

;; Local Variables:
;; coding: utf-8
;; End:

;;; 103-filetype.el ends here
