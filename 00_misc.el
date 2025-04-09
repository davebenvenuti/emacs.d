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
    (let ((vterm-buffer (get-buffer "*vterm*"))
          (vterm-window (get-buffer-window "*vterm*")))
      (if vterm-window
          (delete-window vterm-window)
        (progn
          (split-window-below (- (window-height) 13))
          (other-window 1)
          (if vterm-buffer
              (switch-to-buffer vterm-buffer)
            (vterm))
          (display-line-numbers-mode 0)))))
  (global-set-key (kbd "C-c v") 'my/toggle-vterm-window))

(use-package minimap
  :straight t) ;; minimap (VSCode-like overview)

(use-package rust-mode
  :straight t) ;; Rust mode

(use-package bazel
  :straight (:host github :repo "bazelbuild/emacs-bazel-mode" :files ("bazel.el"))) ;; Bazel mode

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
(global-display-line-numbers-mode t)
(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq lock-file-name-transforms '((".*" "~/.emacs.d/locks/" t)))
(setq native-comp-async-report-warnings-errors 'silent)
