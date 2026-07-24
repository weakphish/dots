;;; weakfish-terminal.el --- Terminal integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Terminal packages and shell-mode integration.

;;; Code:

;; Eat provides a fast terminal emulator and can also improve Eshell's terminal
;; handling without replacing Eshell itself.
(use-package eat
  :commands eat
  :hook (eshell-load . eat-eshell-mode))

(provide 'weakfish-terminal)
;;; weakfish-terminal.el ends here
