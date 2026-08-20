;;; weakfish-coding.el --- Programming tools and language support -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP, documentation, formatting, Tree-sitter, diagnostics, and programming-mode
;; polish.

;;; Code:

;; Apheleia formats through project-standard CLI tools asynchronously.  This is
;; usually more predictable for DevOps work than relying only on LSP formatting.
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'shfmt
        (alist-get 'sh-mode apheleia-mode-alist) 'shfmt
        (alist-get 'python-ts-mode apheleia-mode-alist) 'black
        (alist-get 'python-mode apheleia-mode-alist) 'black
        (alist-get 'typescript-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'tsx-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'json-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'js-json-mode apheleia-mode-alist) 'prettier
        (alist-get 'yaml-ts-mode apheleia-mode-alist) 'prettier
        (alist-get 'yaml-mode apheleia-mode-alist) 'prettier
        (alist-get 'markdown-mode apheleia-mode-alist) 'prettier
        (alist-get 'terraform-mode apheleia-mode-alist) 'terraform))

;; Flymake is built in and is also Eglot's default diagnostics backend.
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode))

;; Highlight matching delimiter pairs in nested code, which makes Lisp and other
;; punctuation-heavy languages easier to scan.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show indentation structure with lightweight guide characters.
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

;; Highlight TODO-style comments without changing how comments are written.
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (text-mode . hl-todo-mode)))

;; Trim trailing whitespace only on lines you touched, avoiding noisy diffs in
;; files that already contain unrelated whitespace.
(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode)))

(provide 'weakfish-coding)
;;; weakfish-coding.el ends here
