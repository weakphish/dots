;;; weakfish-keybindings.el --- Leader keys and discoverability -*- lexical-binding: t; -*-

;;; Commentary:
;; Global helpers and the Space leader map used by Evil states.

;;; Code:

(defun weakfish/open-emacs-config ()
  "Open this Emacs configuration."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun weakfish/reload-emacs-config ()
  "Reload this Emacs configuration without restarting Emacs."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;; General is a thin layer over Emacs keymaps.  It is not required, but it makes
;; a Vim-style leader key much easier to maintain than scattered `define-key'
;; calls.  We use Space in normal/visual states, matching common Vim setups.
(use-package general
  :after evil
  :config
  (general-evil-setup)

  (general-create-definer weakfish/leader-keys
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")

  (weakfish/leader-keys
    "SPC" '(execute-extended-command :which-key "run command")
    "a" '(embark-act :which-key "act here")

    "f" '(:ignore t :which-key "files")
    "f d" '(consult-dir :which-key "directory")
    "f f" '(find-file :which-key "find file")
    "f r" '(consult-recent-file :which-key "recent file")
    "f s" '(save-buffer :which-key "save file")

    "b" '(:ignore t :which-key "buffers")
    "b b" '(consult-buffer :which-key "switch buffer")
    "b d" '(kill-current-buffer :which-key "delete buffer")
    "b h" '(dashboard-open :which-key "home buffer")

    "c" '(:ignore t :which-key "config")
    "c e" '(weakfish/open-emacs-config :which-key "edit config")
    "c p" '(list-packages :which-key "packages")
    "c r" '(weakfish/reload-emacs-config :which-key "reload config")
    "c t" '(weakfish/install-treesit-grammars :which-key "tree-sitter grammars")

    "e" '(:ignore t :which-key "code")
    "e a" '(eglot-code-actions :which-key "code action")
    "e d" '(xref-find-definitions :which-key "definition")
    "e D" '(xref-find-references :which-key "references")
    "e e" '(consult-flymake :which-key "diagnostics")
    "e f" '(apheleia-format-buffer :which-key "format buffer")
    "e F" '(eglot-format :which-key "format selection")
    "e h" '(eldoc-box-help-at-point :which-key "hover docs")
    "e l" '(eglot :which-key "start lsp")
    "e m" '(weakfish/mason-manager :which-key "mason tools")
    "e n" '(flymake-goto-next-error :which-key "next diagnostic")
    "e p" '(flymake-goto-prev-error :which-key "previous diagnostic")
    "e r" '(eglot-rename :which-key "rename symbol")
    "e s" '(eglot-shutdown :which-key "stop lsp")
    "e t" '(:ignore t :which-key "todos")
    "e t n" '(hl-todo-next :which-key "next todo")
    "e t p" '(hl-todo-previous :which-key "previous todo")

    "g" '(:ignore t :which-key "git")
    "g b" '(magit-blame-addition :which-key "blame line")
    "g g" '(magit-status :which-key "git status")
    "g l" '(magit-log-current :which-key "git log")

    "h" '(:ignore t :which-key "help")
    "h c" '(helpful-command :which-key "command help")
    "h f" '(helpful-callable :which-key "function help")
    "h k" '(helpful-key :which-key "key help")
    "h o" '(helpful-at-point :which-key "thing help")
    "h v" '(helpful-variable :which-key "variable help")

    "j" '(:ignore t :which-key "jump")
    "j j" '(avy-goto-char-timer :which-key "jump char")
    "j l" '(avy-goto-line :which-key "jump line")

    "o" '(:ignore t :which-key "org")
    "o a" '(org-agenda :which-key "agenda")
    "o c" '(org-capture :which-key "capture")
    "o j" '(weakfish/open-org-journal :which-key "journal")

    "p" '(:ignore t :which-key "project")
    "p b" '(consult-project-buffer :which-key "project buffer")
    "p c" '(project-compile :which-key "compile project")
    "p d" '(project-dired :which-key "project dired")
    "p f" '(project-find-file :which-key "project file")
    "p k" '(project-kill-buffers :which-key "kill project buffers")
    "p p" '(project-switch-project :which-key "switch project")
    "p s" '(consult-ripgrep :which-key "search project")

    "s" '(:ignore t :which-key "search")
    "s g" '(consult-ripgrep :which-key "grep text")
    "s l" '(consult-line :which-key "search line")

    "t" '(:ignore t :which-key "terminal")
    "t t" '(eat :which-key "terminal")))

;; Which-Key shows available key continuations after a short pause, making the
;; Space leader easier to discover without changing the bindings themselves.
(use-package which-key
  :init
  (setq which-key-idle-delay 0.5)
  :config
  (which-key-mode 1))

(provide 'weakfish-keybindings)
;;; weakfish-keybindings.el ends here
