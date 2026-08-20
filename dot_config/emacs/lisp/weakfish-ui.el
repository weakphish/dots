;;; weakfish-ui.el --- Visual UI packages and theme -*- lexical-binding: t; -*-

;;; Commentary:
;; Icons, home screen, modeline, Dired/completion icon integration, and the
;; active theme.

;;; Code:

;; Nerd Icons supplies glyphs for UI packages.  Iosevka Nerd Font already gives
;; Emacs and the terminal a font that can display them.
(use-package nerd-icons)

;; Dashboard gives Emacs a small home screen without replacing normal completion
;; commands like `consult-recent-file' and `find-file'.
(use-package dashboard
  :unless noninteractive
  :init
  (setq dashboard-startup-banner 'official
        dashboard-center-content t
        dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-items '((recents . 8)
                          (bookmarks . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (nerd-icons-completion-marginalia-setup))

;; Gruvbox is a warm, low-contrast theme that works well for long programming
;; sessions.  The `t' argument marks the theme as trusted so Emacs does not ask
;; for confirmation every startup.
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(provide 'weakfish-ui)
;;; weakfish-ui.el ends here
