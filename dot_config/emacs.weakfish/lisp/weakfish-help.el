;;; weakfish-help.el --- Help and documentation commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Richer help buffers and remapped describe commands.

;;; Code:

;; Helpful replaces the default describe buffers with richer, source-linked
;; explanations while keeping the standard help commands available.
(use-package helpful
  :commands (helpful-callable helpful-command helpful-key
             helpful-variable helpful-at-point)
  :init
  (setq help-window-select t)
  :bind
  (([remap describe-function] . helpful-callable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   ([remap describe-variable] . helpful-variable)))

(provide 'weakfish-help)
;;; weakfish-help.el ends here
