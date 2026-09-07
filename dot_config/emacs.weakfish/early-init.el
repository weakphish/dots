;;; early-init.el --- Settings Emacs reads before init.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs loads this file before package.el and before it creates the first
;; graphical frame.  Keep this file small: it is for startup mechanics, not for
;; day-to-day editor behavior.

;;; Code:

;; package.el normally initializes installed packages before init.el runs.  We
;; disable that so init.el can set package archives first and then initialize
;; packages in a predictable order.
(setq package-enable-at-startup nil)

;; Reduce garbage collection during startup.  Emacs allocates a lot while
;; loading packages; collecting less often makes startup smoother.  init.el
;; restores a normal value after startup finishes.
(setq gc-cons-threshold most-positive-fixnum)

;; Native compilation warnings from third-party packages are usually not
;; actionable while editing; keep them out of the UI but still report errors.
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

;; Avoid a flash of the default toolbar/menu/scrollbar before our theme loads.
;; These settings affect the initial frame, which is why they live here instead
;; of in init.el.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;;; early-init.el ends here
