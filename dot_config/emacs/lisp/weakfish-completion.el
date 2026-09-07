;;; weakfish-completion.el --- Completion and navigation -*- lexical-binding: t; -*-

;;; Commentary:
;; Minibuffer and in-buffer completion, history, recent files, and jump/search
;; commands.

;;; Code:

;; Corfu provides an in-buffer completion popup for Emacs' standard
;; `completion-at-point' system, which Eglot and many modes already use.
(use-package corfu
  :init
  (setq tab-always-indent 'complete
        corfu-cycle t
        corfu-auto t)
  (global-corfu-mode 1))

;; Cape adds small, reusable completion sources.  Appending keeps mode-specific
;; sources such as Eglot ahead of these general fallbacks.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(use-package savehist
  :ensure nil
  :init
  (setq savehist-file (expand-file-name "savehist" weakfish/cache-directory))
  (savehist-mode 1))

(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file (expand-file-name "recentf" weakfish/cache-directory)
        recentf-max-saved-items 200)
  :config
  (recentf-mode 1))

;; Provides minimal vertical completion UI, based on the defaults.
(use-package vertico
  :init
  (vertico-mode 1))

;; Orderless matches space-separated completion patterns in any order.  File
;; completion keeps partial-completion so paths like `src/foo' still work well.
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia annotates minibuffer candidates with useful context such as
;; command docs, file metadata, and buffer modes.
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

;; Consult provides practical navigation and search commands on top of Emacs'
;; standard completion system.
(use-package consult)

;; Consult-dir lets minibuffer file commands jump between useful directories,
;; including project roots, recent directories, and open buffers' directories.
(use-package consult-dir
  :after (consult vertico)
  :bind (:map vertico-map
              ("C-x C-d" . consult-dir)
              ("C-x C-j" . consult-dir-jump-file)))

;; Embark turns the current thing or minibuffer candidate into an action target,
;; like a contextual command palette that composes with Consult completions.
(use-package embark
  :bind
  (("C-." . embark-act))
  :commands embark-act)

;; Let Embark collection buffers use Consult's live preview when both packages
;; are available.
(use-package embark-consult
  :after (consult embark)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Avy gives fast visible jumps to characters and lines without replacing Evil's
;; normal motions.
(use-package avy
  :commands (avy-goto-char-timer avy-goto-line))

(provide 'weakfish-completion)
;;; weakfish-completion.el ends here
