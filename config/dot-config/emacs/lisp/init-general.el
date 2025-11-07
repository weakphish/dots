(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(execute-extended-command :which-key "execute command")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "b d"  '(kill-current-buffer :which-key "Kill Buffer")

    ;; Project switching
    "p" '(:ignore t :which-key "project")
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")))


(provide 'init-general)
