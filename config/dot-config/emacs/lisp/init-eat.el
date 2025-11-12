;; eat: Emulate A Terminal (https://codeberg.org/akib/emacs-eat)
(use-package eat
    :preface
    (defun my--eat-open (file)
        "Helper function to open files from eat terminal."
        (interactive)
        (if (file-exists-p file)
                (find-file-other-window file t)
            (warn "File doesn't exist")))
    :init
    (add-to-list 'project-switch-commands '(eat-project "Eat terminal") t)
    (add-to-list 'project-switch-commands '(eat-project-other-window "Eat terminal other window") t)
    (add-to-list 'project-kill-buffer-conditions '(major-mode . eat-mode))
    :config
    (add-to-list 'eat-message-handler-alist (cons "open" 'my--eat-open))
    (setq process-adaptive-read-buffering nil) ; makes EAT a lot quicker!
    (setq eat-term-name "xterm-256color") ; https://codeberg.org/akib/emacs-eat/issues/119"
    (setq eat-kill-buffer-on-exit t)
    (setq eat-shell-prompt-annotation-failure-margin-indicator "")
    (setq eat-shell-prompt-annotation-running-margin-indicator "")
    (setq eat-shell-prompt-annotation-success-margin-indicator "")
    :general
    (leader-keys
      "e" '(:ignore t :which-key "eat")
      "e e" '(eat :which-key "eat")
      "e E" '(eat-other-window :which-key "eat other window")
      "e p" '(eat-project :which-key "eat (project)")
      "e P" '(eat-project-other-window :which-key "eat (project) other window")))

