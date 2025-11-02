;; PACKAGE LIST:
;; Evil recommends installation via package.el
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)

(provide 'init-evil)
