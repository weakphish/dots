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
    "s" '(:ignore t :which-key "search")            
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

(provide 'init-consult)
