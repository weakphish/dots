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
    ;; Misc keybinds that don't fit into a use-package declaration
    "x" '(execute-extended-command :which-key "execute command")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    "w" '(:ignore t :which-key "window")
    "w s" '(window-configuration-to-register :which-key "window config to register")
    "w l" '(jump-to-register :which-key "load register configuration")

    ;; Don't show an error because SPC b ESC is undefined, just abort
    "b <escape>" '(keyboard-escape-quit :which-key t)
    "b d"  '(kill-current-buffer :which-key "Kill Buffer")

    ;; Project switching
    "p" '(:ignore t :which-key "project")
    "p p" '(project-switch-project :which-key "switch project")))
    ;;"p a" '(projectile-add-known-project :which-key "add project")))


(provide 'init-general)
