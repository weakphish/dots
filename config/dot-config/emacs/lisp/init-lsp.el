;;; lsp.el --- LSP/Eglot configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Language Server Protocol helpers and bindings separated from init.el.

;;; Code:

(require 'cl-lib)

;;; Functions
(defun mode-has-lsp-p ()
  "Check if MODE (or current major-mode) has an LSP server configured in Eglot."
  (require 'eglot nil t)
  (and (buffer-file-name)
       (ignore-errors (cl-fourth (eglot--guess-contact)))))

;;; Packages
(use-package emacs
  :hook (zig-mode . eglot-ensure)
  :hook (rust-mode . eglot-ensure)
  :hook (go-mode . eglot-ensure)
  :hook (typescript-mode . eglot-ensure)
  :general
  (leader-keys
    "l" '(:ignore t :which-key "lsp")
    "l <escape>" '(keyboard-escape-quit :which-key t)
    "l r" '(eglot-rename :which-key "rename")
    "l a" '(eglot-code-actions :which-key "code actions")))

(provide 'init-lsp)
