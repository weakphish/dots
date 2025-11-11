;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helpful resources:
;;
;;  - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  :hook
  (((python-mode python-ts-mode go-mode go-ts-mode) . eglot-ensure))

  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t) ; activate Eglot in referenced non-project files

  :config
  ;; Configure Python to use BasedPyright
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
  		 "basedpyright-langserver" "--stdio"))

  ;; Configure Go to have inlay hints provided.
  ;; As seen here: https://www.reddit.com/r/emacs/comments/11bqzvk/comment/ja03s4w/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) .
					("gopls" :initializationOptions
					 (:hints (:parameterNames t
								  :rangeVariableTypes t
								  :functionTypeParameters t
								  :assignVariableTypes t
								  :compositeLiteralFields t
								  :compositeLiteralTypes t
								  :constantValues t)))))


  (fset #'jsonrpc--log-event #'ignore) ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
					; (add-to-list 'eglot-server-programs
					;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  :general
  (leader-keys
    "c" '(:ignore t :which-key "code")
    "c a" '(eglot-code-actions :which-key "actions")
    "c f" '(eglot-format-buffer :which-key "format buffer")
    "c i" '(eglot-find-implementation :which-key "implementation")
    "c d" '(eglot-find-declaration :which-key "declaration")
    "c y" '(eglot-find-typeDefinition :which-key "type definition")))


(provide 'init-lsp)
