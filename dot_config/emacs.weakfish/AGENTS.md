# Agent Notes

## Repo Shape
- This is a compact Emacs config, not an app: the startup entrypoints are `early-init.el` and `init.el`.
- `early-init.el` is only for startup mechanics that must run before `init.el` or before the first frame exists.
- `init.el` is a small loader. Purpose-based hand-written config lives under `lisp/` as `weakfish-*` modules; keep related setup together in the relevant module rather than growing `init.el`.
- The config intentionally documents Elisp for experienced developers coming from Neovim; keep additions clear and modern without turning comments into a tutorial.
- `custom.el` is Emacs Custom output loaded from `init.el`; avoid hand-editing it unless the task is specifically about Custom-managed settings.
- `elpa/` contains installed package artifacts. Do not edit vendored package code unless intentionally updating or debugging those packages.

## Style And Conventions
- Keep the config focused, maintainable, and small; prefer one understandable `use-package` block over scattered setup.
- Preserve `lexical-binding: t` file headers on hand-written Elisp files.
- Put package setup in the relevant `lisp/` module with `use-package`; set variables that must affect package loading in `:init`, and post-load setup in `:config`.
- Keep the Neovim transition smooth: Evil and `general` leader mappings are core UX, with `SPC` as the leader in normal/visual/motion states.
- Comments should explain Emacs/Elisp concepts an experienced software engineer new to Emacs would otherwise trip over.

## Verification
- Load the repo-local config with:
  `emacs --batch -Q --eval '(setq user-emacs-directory (file-name-as-directory default-directory))' --load early-init.el --load init.el --eval '(message "repo-config-ok")'`
- The `user-emacs-directory` override matters; without it, batch Emacs may read packages and `custom.el` from the user's default Emacs directory instead of this repo.
- Avoid leaving generated root bytecode files such as `init.elc` or `early-init.elc` in the repo after experiments.

## Package Notes
- Package archives are set in `init.el` to GNU ELPA, NonGNU ELPA, and MELPA before `package-initialize`.
- `use-package-always-ensure` is enabled, so adding a new `use-package` form can install packages on first startup and may require network access.
- Current first-party configured packages are `evil`, `evil-collection`, `general`, and `gruvbox-theme`.
