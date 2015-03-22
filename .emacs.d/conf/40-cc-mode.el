(font-lock-add-keywords
 'c-mode '(
					 ("\\<m?i_[a-zA-Z0-9]+\\>" . 'font-lock-variable-name-face)
					 ("\\<Intcrl\\(Enable\\|Disable\\)AllInterrupt\\>()" . 'font-lock-keyword-face)
					 ("\\<MYSTATIC\\>" . 'font-lock-keyword-face)
					 ("\\<\\(ST\\|EM\\)_[a-zA-Z0-9_]+\\>" . 'font-lock-type-face)
					 ("\\<[A-Z][A-Z0-9_]+[A-Z0-9]\\>" . 'font-lock-constant-face)
					 ("\\<\\(VALID\\|INVALID\\)\\>" . 'font-lock-constant-face)
					 ))
