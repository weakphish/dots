;;; weakfish-package.el --- Package bootstrap and shell environment -*- lexical-binding: t; -*-

;;; Commentary:
;; Package archives and `use-package' must be ready before the rest of the config
;; can declare package setup.

;;; Code:

(require 'package)

;; MELPA contains most modern community packages.  GNU ELPA and NonGNU ELPA are
;; official package archives and are useful fallbacks for core packages.
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Emacs 29+ ships `use-package'.  This fallback keeps the config usable on an
;; older Emacs as long as package.el can install it.
(unless (package-installed-p 'use-package)
  (unless package-archive-contents
    (package-refresh-contents))
  (package-install 'use-package))

(require 'use-package)
(require 'use-package-ensure)

;; Avoid writing `:ensure t' on every package.  Packages are installed on first
;; startup and then loaded from disk afterward.
(setq use-package-always-ensure t)

;; macOS GUI Emacs is usually launched outside your login shell, so it does not
;; see PATH changes from shell startup files unless we import them explicitly.
(use-package exec-path-from-shell
  :if (or (memq window-system '(mac ns x))
          (daemonp))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH"))
  :config
  (exec-path-from-shell-initialize))

(provide 'weakfish-package)
;;; weakfish-package.el ends here
