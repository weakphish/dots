;;; completion.el --- Minibuffer and completion configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Extracted from init.el to keep completion tweaks in one place.

;; Minibuffer/completion settings
(setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
(setopt completions-detailed t)                        ; Show annotations
(setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setopt completions-max-height 20)                     ; This is arbitrary
(setopt completions-format 'one-column)
(setopt completions-group t)
(setopt completion-auto-select 'second-tab)            ; Much more eager
;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode,
;; icomplete-vertical, or fido-mode. See also the file extras/base.el

;(icomplete-vertical-mode)
;(fido-vertical-mode)
;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu: Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind ;; TODO: general-ize
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  ;; Enable auto completion, configure delay, trigger and quitting
  (corfu-auto t))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Avy style selection
;;(use-package corfu-quick
;;  :after corfu
;;  :hook (corfu-mode . corfu-quick-mode)
;;  :ensure nil)

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :general 
  (leader-keys
    "b" '(:ignore t :which-key "buffers")            
    "b b" '(consult-buffer :which-key "buffers")     
    "s g"  '(consult-ripgrep :which-key "grep")
    "s r"  '(consult-register :which-key "registers")
    "s j"  '(evil-collection-consult-jump-list :which-key "jump list")
    "f" '(:ignore t :which-key "files")            
    "f f"  '(consult-find :which-key "files"))
    ;;("M-y"   . consult-yank-pop)   ; orig. yank-pop
    ;; Searching
    ;;("M-s l" . consult-line)       ; Alternative: rebind C-s to use
    ;;("M-s s" . consult-line)       ; consult-line instead of isearch, bind
    ;;("M-s L" . consult-line-multi) ; isearch to M-s s
    ;;("M-s o" . consult-outline)
    ;;;; Isearch integration
    ;;:map isearch-mode-map
    ;;("M-e" . consult-isearch-history)   ; orig. isearch-edit-string
    ;;("M-s e" . consult-isearch-history) ; orig. isearch-edit-string
    ;;("M-s l" . consult-line)            ; needed by consult-line to detect isearch
    ;;("M-s L" . consult-line-multi)      ; needed by consult-line to detect isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark-consult
  :ensure t)

;; Embark: supercharged context-dependent menu; kinda like a
;; super-charged right-click.
(use-package embark
  :ensure t
  :demand t
  :after (avy embark-consult)
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(provide 'init-completion)
