(use-package rg
  :straight t) ;; ripgrep

(use-package vterm
  :straight t) ;; better terminal

;; Set various modes, hooks and variables here
(menu-bar-mode -1)
(xterm-mouse-mode 1)
(global-display-line-numbers-mode t)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq native-comp-async-report-warnings-errors 'silent)
