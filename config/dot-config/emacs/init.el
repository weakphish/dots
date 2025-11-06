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
(require 'init-evil) 
(require 'init-general)

(require 'init-ui)

(require 'init-functions)

(require 'init-completion)
(require 'init-treemacs)
(require 'init-magit)
(require 'init-avy)
(require 'init-lsp)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(image-load-path
   '("~/.config/emacs/calle24/images"
     "/Applications/Emacs.app/Contents/Resources/etc/images/"
     data-directory load-path))
 '(package-selected-packages
   '(avy calle24 cape corfu-terminal csv-mode diff-hl doom-modeline
	 doom-themes eat embark-consult evil-collection general
	 go-mode helpful json-mode kind-icon lua-mode magit marginalia
	 markdown-mode orderless rainbow-delimiters rust-mode treemacs
	 treemacs-evil treemacs-icons-dired treemacs-magit
	 treemacs-persp treemacs-projectile treemacs-tab-bar
	 treesit-auto typescript-mode vertico wgrep yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
