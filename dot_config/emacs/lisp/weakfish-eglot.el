;;; weakfish-eglot.el --- Eglot setup and support -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP, documentation.

;;; Code:

;; Eglot is the built-in LSP client.  It uses ordinary Emacs completion, xref,
;; and Flymake, so it works with Corfu, Consult, and the existing completion UI.
(use-package eglot
  :ensure nil
  :hook ((bash-ts-mode . eglot-ensure)
         (dockerfile-ts-mode . eglot-ensure)
         (hcl-ts-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (markdown-mode . eglot-ensure)
         (markdown-ts-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (terraform-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure))
  :init
  (setq eglot-autoshutdown t)
  :config
  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode) .
                 ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((python-ts-mode python-mode) .
                 ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((yaml-ts-mode yaml-mode) .
                 ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((bash-ts-mode sh-mode) .
                 ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs
               '((json-ts-mode js-json-mode json-mode) .
                 ("vscode-json-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((dockerfile-ts-mode dockerfile-mode) .
                 ("docker-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((hcl-ts-mode terraform-mode) .
                 ("terraform-ls" "serve")))
  (add-to-list 'eglot-server-programs
               '((markdown-mode markdown-ts-mode gfm-mode) .
                 ("marksman" "server"))))

;; Show Eldoc documentation in a small child-frame popup, closer to Neovim's
;; floating documentation UI than the default echo area.
(use-package eldoc-box
  :commands (eldoc-box-help-at-point)
  :config
  (add-hook 'eldoc-box-buffer-setup-hook #'eldoc-box-prettify-ts-errors 0 t)
  (setq eldoc-box-clear-with-C-g t))

;; mason.el manages external language tools without coupling Eglot startup to
;; tool installation.  Open `mason-manager' when you want to inspect or install
;; servers, linters, and formatters.
(use-package mason
  :defer t
  :init
  (setq mason-dir (expand-file-name "mason/" weakfish/cache-directory))
  :commands (mason-ensure mason-manager mason-install mason-doctor mason-log))

(defun weakfish/mason-manager ()
  "Open Mason's tool manager after preparing Mason."
  (interactive)
  (require 'mason)
  (mason-ensure #'mason-manager))

(provide 'weakfish-eglot)
;;; weakfish-eglot.el ends here
