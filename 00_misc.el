(use-package rg
  :straight t) ;; ripgrep

;; C-c C-t copy mode (navigate terminal output)
(use-package vterm
  :straight t) ;; better terminal

(use-package minimap
  :straight t) ;; minimap (VSCode-like overview)

(use-package rust-mode
  :straight t) ;; Rust mode

(use-package bazel
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode" :files ("bazel.el"))) ;; Bazel mode

(defun my/display-line-numbers-except-some-modes ()
  "Disable line numbers for specific buffers."
  (when (or (minibufferp)
            (string-match-p "^\\*vterm\\*" (buffer-name))
            ;; Add more conditions here for other buffers
            )
    (display-line-numbers-mode -1)))

(add-hook 'display-line-numbers-mode-hook 'my/display-line-numbers-except-some-modes)

;; Set various modes, hooks and variables here
(menu-bar-mode -1)
(xterm-mouse-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode t)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs.d/locks/" t)))
(setq native-comp-async-report-warnings-errors 'silent)

(custom-set-faces
 '(minibuffer-prompt ((t (:inherit default :foreground "cyan")))))
