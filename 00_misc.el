(use-package rg
  :straight t) ;; ripgrep

;; C-c C-t copy mode (navigate terminal output)
(use-package vterm ;; better terminal
  :straight t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-use-vterm-color t)
  (setq vterm-kill-buffer-on-exit t)
  (defun my/toggle-vterm-window ()
    "Toggle a vterm window at the bottom of the screen with height 13."
    (interactive)
    (let ((vterm-buffer-name "*vterm (lower)*"))
      (let ((vterm-buffer (get-buffer vterm-buffer-name))
            (vterm-window (get-buffer-window vterm-buffer-name)))
        (if vterm-window
            (delete-window vterm-window)
          (progn
            (split-window-below (- (window-height) 13))
            (other-window 1)
            (if vterm-buffer
		(switch-to-buffer vterm-buffer)
              (progn
                (vterm vterm-buffer-name)
                (display-line-numbers-mode 0))))))))
  (global-set-key (kbd "C-c v") 'my/toggle-vterm-window))

(use-package minimap
  :straight t) ;; minimap (VSCode-like overview)

(use-package kotlin-mode
  :ensure t    ;; Install the package if not already installed
  :defer t     ;; Defer loading until needed
  :mode "\\.kt\\'")

(use-package rust-mode
  :straight t) ;; Rust mode

(use-package protobuf-mode
  :straight t
  :mode "\\.proto\\'") ;; Protocol Buffers mode

(use-package json-ts-mode
  :mode "\\.ejson\\'") ;; JSON mode for .ejson files

(use-package bazel
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode" :files ("bazel.el"))) ;; Bazel mode

(use-package ag
  :straight t) ;; Silver Searcher (ag) mode

;; Set UTF-8 as the default encoding
(dolist (mode '(vterm-mode
                term-mode
                shell-mode
                eshell-mode))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode 0))))

;; Set various modes, hooks and variables here
(menu-bar-mode -1)
(xterm-mouse-mode 0)
(column-number-mode 1)
;; This works better than global-display-line-numbers-mode apparently
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs.d/locks/" t)))
(setq native-comp-async-report-warnings-errors 'silent)
