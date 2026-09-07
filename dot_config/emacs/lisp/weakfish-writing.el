;;; weakfish-writing.el --- Writing and Org configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Org setup, capture templates, journal helper, and prose polish.

;;; Code:

;; Org is built in; keep the setup minimal and let `org-modern' handle polish.
(use-package org
  :ensure nil ;; Loads the package using package.el if needed
  :commands ;; Define autoloads for commands defined by the package
  (org-agenda org-capture)
  :hook ;; Hooks to attach this package to
  ((org-agenda-mode . weakfish/disable-eldoc-in-org-agenda)
   (org-mode . weakfish/hide-journal-tree))
  :init ;; Code to run before PACKAGE is loaded
  ;; Set the Org directory and slurp them all up as agenda files
  (setq org-directory (file-name-as-directory (expand-file-name "~/org"))
        org-agenda-files (file-expand-wildcards
                          (expand-file-name "*.org" org-directory))
        org-startup-indented t ;; Indent headings logically
        org-hide-emphasis-markers t  ;; Hide emphasis marks; *foo* becomes just foo
        org-pretty-entities t ;; Pretty symbols from LaTeX like \alpha
        org-default-priority ?D ;; Default priority for a new item
        org-lowest-priority ?D ;; Lowest priority
        org-priority-faces ;; Set custom color/boldness for priority levels
        '((?A . (:foreground "#fb4934" :weight bold))
          (?B . (:foreground "#fabd2f" :weight bold))
          (?C . (:foreground "#83a598" :weight bold))
          (?D . (:foreground "#b8bb26" :weight bold)))
        org-agenda-sorting-strategy;; Sort agenda by priorty/time
        '((agenda priority-down time-up)
          (todo priority-down)
          (tags priority-down)
          (search priority-down))
        org-capture-templates ;; Capture templates
        '(("t" "Task" entry
           (file+headline "tasks.org" "Tasks")
           "* TODO %?\n%U\n")
          ("j" "Journal" entry
           (file+datetree "journal.org")
           "* %?\n%U\n"))))

(defun weakfish/disable-eldoc-in-org-agenda ()
  "Disable Eldoc in Org Agenda buffers."
  (eldoc-mode -1))

(defun weakfish/hide-journal-tree ()
  "Fold journal.org when opening it."
  (when (and buffer-file-name
             (file-equal-p buffer-file-name
                           (expand-file-name "journal.org" org-directory)))
    (org-overview)))

(defun weakfish/open-org-journal ()
  "Open the Org journal file."
  (interactive)
  (require 'org)
  (find-file (expand-file-name "journal.org" org-directory)))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(provide 'weakfish-writing)
;;; weakfish-writing.el ends here
