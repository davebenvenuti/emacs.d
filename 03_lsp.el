(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GEM_HOME")
  (exec-path-from-shell-copy-env "GEM_PATH")
  (exec-path-from-shell-copy-env "RUBY_VERSION"))

(defun my/find-compile-commands-dir ()
  "Locate the directory containing the 'compile_commands.json' file and print it."
  (let ((project-root (locate-dominating-file default-directory "compile_commands.json")))
    (if project-root
        (progn
          (message "Found compile_commands.json in directory: %s" project-root)
          project-root)
      (message "compile_commands.json not found in any parent directory.")
      nil)))

;; Assign the directory to a variable and print it.
(setq my-compile-commands-dir (my/find-compile-commands-dir))

;; M-? allows you to navigate references etc
;; (with-eval-after-load 'eglot
;;   (add-hook 'eglot-managed-mode-hook
;;             (lambda ()
;;               (let ((root (locate-dominating-file
;;                            default-directory
;;                            "compile_commands.json")))
;;                 (when root
;;                   (setq default-directory root))))))

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
;;   (add-to-list 'eglot-server-programs
;;                `(c++-mode 2. ("clangd" (concat "--compile-commands-dir=" my-compile-commands-dir) "--query-driver=/**/*"))))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; Use ruby-lsp not treesitter
  (delete 'ruby treesit-auto-langs)
  ;; Explicitly remove ruby-ts-mode from auto-mode-alist
  (setq auto-mode-alist (delete '("\\.rb\\'" . ruby-ts-mode) auto-mode-alist))
  (global-treesit-auto-mode))

;; Make sure we use ruby-mode, not ruby-ts-mode
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

(use-package lsp-mode
  :bind
  ("M-RET" . lsp-execute-code-action)
  :hook
  (ruby-mode . lsp-deferred)
  :config
  (setq lsp-disabled-clients '(rubocop-ls sorbet-ls semgrep-ls)))
;; Note: there's also an lsp-enabled-clients

;; See also: 06_debug.el
