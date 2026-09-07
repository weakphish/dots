;;; weakfish-coding.el --- Programming tools and language support -*- lexical-binding: t; -*-

;;; Commentary:
;; LSP, documentation, formatting, Tree-sitter, diagnostics, and programming-mode
;; polish.

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

;; Tree-sitter is built into Emacs 29+, but grammars are external native
;; libraries.  This teaches Emacs where to fetch them without installing them on
;; every startup; run `weakfish/install-treesit-grammars' when you want to install.
(use-package treesit
  :ensure nil
  :when (fboundp 'treesit-available-p)
  :init
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                    "split_parser" "tree-sitter-markdown/src")
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                           "split_parser" "tree-sitter-markdown-inline/src")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  :config
  (defun weakfish/install-treesit-grammars ()
    "Install configured Tree-sitter grammars that are missing."
    (interactive)
    (unless (treesit-available-p)
      (user-error "Tree-sitter is not available in this Emacs build"))
    (dolist (language (mapcar #'car treesit-language-source-alist))
      (unless (treesit-language-available-p language)
        (treesit-install-language-grammar language))))

  (defun weakfish/prefer-treesit-mode (base-mode treesit-mode language &rest patterns)
    "Use TREESIT-MODE for BASE-MODE and PATTERNS when LANGUAGE is installed."
    (when (and (fboundp treesit-mode)
               (treesit-language-available-p language))
      (add-to-list 'major-mode-remap-alist `(,base-mode . ,treesit-mode))
      (dolist (pattern patterns)
        (add-to-list 'auto-mode-alist `(,pattern . ,treesit-mode)))))

  (weakfish/prefer-treesit-mode 'sh-mode 'bash-ts-mode 'bash)
  (weakfish/prefer-treesit-mode 'python-mode 'python-ts-mode 'python)
  (weakfish/prefer-treesit-mode 'typescript-mode 'typescript-ts-mode 'typescript "\\.ts\\'")
  (weakfish/prefer-treesit-mode 'tsx-mode 'tsx-ts-mode 'tsx "\\.tsx\\'")
  (weakfish/prefer-treesit-mode 'js-json-mode 'json-ts-mode 'json "\\.jsonc?\\'")
  (weakfish/prefer-treesit-mode 'yaml-mode 'yaml-ts-mode 'yaml "\\.ya?ml\\'")
  (weakfish/prefer-treesit-mode 'dockerfile-mode 'dockerfile-ts-mode 'dockerfile
				"\\(?:Containerfile\\|Dockerfile\\)\\(?:\\..*\\)?\\'")

  ;; Terraform uses HCL syntax.  Prefer a Tree-sitter HCL mode if this Emacs
  ;; provides one; otherwise use `terraform-mode' below.
  (when (and (fboundp 'hcl-ts-mode)
             (treesit-language-available-p 'hcl))
    (dolist (pattern '("\\.hcl\\'" "\\.tf\\'" "\\.tfvars\\'"))
      (add-to-list 'auto-mode-alist `(,pattern . hcl-ts-mode))))

  (when (and (fboundp 'markdown-ts-mode)
             (treesit-language-available-p 'markdown))
    (dolist (pattern '("\\.md\\'" "\\.markdown\\'"))
      (add-to-list 'auto-mode-alist `(,pattern . markdown-ts-mode)))))

;; Fallbacks and modes not covered well by built-in Tree-sitter modes.
(use-package yaml-mode
  :if (not (fboundp 'yaml-ts-mode))
  :mode "\\.ya?ml\\'")

(use-package dockerfile-mode
  :if (not (fboundp 'dockerfile-ts-mode))
  :mode "\\(?:Containerfile\\|Dockerfile\\)\\(?:\\..*\\)?\\'")

(use-package terraform-mode
  :if (not (fboundp 'hcl-ts-mode))
  :mode ("\\.tf\\'" "\\.tfvars\\'"))

(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'"))

;; Helm chart templates are YAML plus Go templates, so plain YAML parsers often
;; choke on `{{ ... }}'.  Web Mode handles template delimiters pragmatically.
(use-package web-mode
  :mode ("/templates/.*\\.ya?ml\\'" . web-mode)
  :config
  (add-to-list 'web-mode-engines-alist '("go" . "/templates/.*\\.ya?ml\\'")))

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
