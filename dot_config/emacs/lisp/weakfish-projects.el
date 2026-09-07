;;; weakfish-projects.el --- Git and project integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Git porcelain, change indicators, and built-in project commands.

;;; Code:

;; Magit is the standard Emacs Git porcelain: status, staging, committing,
;; branching, and history from inside Emacs.
(use-package magit)

;; diff-hl shows Git changes in the fringe or margin as you edit, similar to
;; signcolumn Git indicators in Vim.
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (text-mode . diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  (diff-hl-flydiff-mode 1)
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; `project' is built into Emacs.  It provides project roots and project-scoped
;; commands that integrate cleanly with Consult completion.
(use-package project
  :ensure nil
  :init
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-project-buffer "Buffer")
          (consult-ripgrep "Search")
          (project-dired "Dired"))))

(provide 'weakfish-projects)
;;; weakfish-projects.el ends here
