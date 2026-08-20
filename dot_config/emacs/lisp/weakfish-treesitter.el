;;; weakfish-treesitter.el --- Treesitter setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-sitter setup and helpers.
;;; Code:

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
;; Match tree-sitter mode to stuff like .yml/.yaml, Dockerfile.*, Containerfile, etc...
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

(provide 'weakfish-treesitter)
;;; weakfish-treesitter.el ends here
