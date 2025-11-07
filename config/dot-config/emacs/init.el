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

;; Put Custom vars in their own jail (file)
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Load my other modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.config/emacs/lisp/")

(require 'init-base)
(require 'init-general)

(require 'init-org)
(require 'init-evil) 

(require 'init-ui)

(require 'init-functions)

(require 'init-completion)
(require 'init-treemacs)
(require 'init-magit)
(require 'init-avy)
(require 'init-lsp)

