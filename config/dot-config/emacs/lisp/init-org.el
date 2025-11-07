;; Agenda variables
(setq org-directory "~/org/") ; Non-absolute paths for agenda and capture templates will look here.

(setq org-agenda-files '("inbox.org" "work-log.org" "todo.org"))

;; Default tags
(setq org-tag-alist
      '(
        ;; locale
	(:startgroup)
	("work" . ?w)
	(:endgroup)
	(:newline)
	;; scale
	(:startgroup)
	("one-shot" . ?o)
	("project" . ?j)
	("tiny" . ?t)
	(:endgroup)
	;; misc
	("meta")
	("review")
	("reading")))

;; Org-refile: where should org-refile look?
(setq org-refile-targets '(
			   ("todo.org" :maxlevel . 3)
			   ("work-log.org" :maxlevel . 9)))

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))    ; spell checking!

  :general
  (leader-keys
    "o" '(:ignore t :which-key "org")
    "o c" '(org-capture :which-key "capture")
    "o a" '(org-agenda :which-key "agenda")
    "o l" '(:ignore t :which-key "links")
    "o l s" '(org-store-link)        
    "o l i" '(org-insert-link-global))

  :config
  (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in. Run
  ;;     M-x describe-variable RET org-todo-keywords RET
  ;; for documentation on how these keywords work.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
    '(
	("c" "Default Capture" entry (file "inbox.org") "* %?\n%U\n%i")
	;; Capture and keep an org-link to the thing we're currently working with
	("r" "Capture with Reference" entry (file "inbox.org") "* TODO %?\n%U\n%i\n%a")
	;; Define a section
	("w" "Work")
	("wm" "Work Meeting" entry (file+olp+datetree "work-log.org") "* Meeting - %?\n%T\n")
	("wj" "Work Log Entry" entry (file+olp+datetree "work-log.org") "* %?\n%U\n%i\n%a" :empty-lines 0)
	("wt" "TODO" entry (file "inbox.org") "* TODO %?\n%U\n%i")))

    ;; An agenda view lets you see your TODO items filtered and
    ;; formatted in different ways. You can have multiple agenda views;
    ;; please see the org-mode documentation for more information.
    (setq org-agenda-custom-commands
	'(
	    ("n" "Agenda and All Todos"
		(
		    (agenda)
		    (todo)
		    )
	    )
	    ("w" "Work" agenda ""
		(
		    (org-agenda-files
			'("work.org")
		     )
		)
	    )
	)
    )
)

(provide 'init-org)
