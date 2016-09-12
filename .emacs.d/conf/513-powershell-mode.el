;;; Code:
;; autoload powershell interactive shell
(autoload 'powershell "powershell" "Start a interactive shell of PowerShell." t)

;; powershell-mode
(autoload 'powershell-mode "powershell" "A editing mode for Microsoft PowerShell." t)
(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode)) ; PowerShell script

;; Local Variables:
;; coding: utf-8
;; End:

;;; 111-c-mode.el ends here
