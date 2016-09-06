;;; Code:
(autoload 'visual-basic-mode "visual-basic-mode" "Visual Basic mode." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|rvb\\)$" .
                                  visual-basic-mode)) auto-mode-alist))

(setq visual-basic-mode-indent 4)
(add-hook 'visual-basic-mode-hook '(lambda () (setq indent-tabs-mode 'nil)))

;; Local Variables:
;; coding: utf-8
;; End:

;;; 111-c-mode.el ends here
