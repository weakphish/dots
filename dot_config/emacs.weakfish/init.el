;;; init.el --- Small, modern Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is the entrypoint.  Purpose-specific setup lives in `lisp/' modules
;; so each area can be understood and changed without scanning the whole config.

;;; Code:

(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory (file-truename load-file-name))))

(require 'weakfish-package)
(require 'weakfish-core)
(require 'weakfish-completion)
(require 'weakfish-projects)
(require 'weakfish-terminal)
(require 'weakfish-writing)
(require 'weakfish-coding)
(require 'weakfish-vim)
(require 'weakfish-help)
(require 'weakfish-ui)
(require 'weakfish-keybindings)

;;; init.el ends here
