;;; Code:
(require 'howm)
(setq howm-directory (concat user-emacs-directory "howm"))
(setq howm-menu-lang 'ja)
(setq howm-file-name-format "%Y-%m.howm")
(add-to-list 'load-path "elisp/howm")
(define-key global-map "\C-c,," 'howm-menu)

;; Local Variables:
;; coding: utf-8
;; End:

;;; 399-howm.el ends here
