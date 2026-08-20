;;; weakfish-vim.el --- Vim-style editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Evil and Evil Collection provide the modal editing baseline.

;;; Code:

;; Evil is the de facto Vim emulation layer for Emacs.  It gives you Vim's modal
;; editing model while still letting you use Emacs packages and commands.
(use-package evil
  :init
  ;; These variables must be set before Evil loads.
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Evil Collection teaches many built-in and third-party Emacs modes how to use
;; Vim-style keys.  Without it, Evil works in normal buffers but feels uneven in
;; places like Dired, help buffers, and package menus.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(provide 'weakfish-vim)
;;; weakfish-vim.el ends here
