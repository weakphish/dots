;;; Guardrail
(when (< emacs-major-version 29)
  (error "Emacs Bedrock only works with Emacs 29 and newer; you have version %s" emacs-major-version))

;; First-time setup
(unless (file-exists-p "~/.config/emacs/.first-run")
  (write-region "" nil "~/.config/emacs/.first-run")
  (run-with-timer 1 nil
                  (lambda ()
		    (ignore-errors (kill-buffer "*Compile-Log*"))
		    (delete-other-windows)
                    (message "First-time setup complete. Please restart Emacs. Closing in 10 seconds")
		    (run-with-timer 10 nil #'save-buffers-kill-emacs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Load my other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.config/emacs/lisp/")
(require 'init-base)
(require 'init-evil) 
(require 'init-general)
(require 'init-functions)
(require 'init-ui)
(require 'init-completion)
(require 'init-avy)
(require 'init-lsp)
