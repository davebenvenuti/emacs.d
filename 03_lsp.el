(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GEM_HOME")
  (exec-path-from-shell-copy-env "GEM_PATH")
  (exec-path-from-shell-copy-env "RUBY_VERSION"))

(use-package eglot
  :ensure nil  ;; Use built-in version
  :commands eglot
  :bind (:map eglot-mode-map
         ("M-RET" . eglot-code-actions)
         ("C-c a" . eglot-code-actions-at-point))
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (ruby-mode . eglot-ensure)
         ;; Add other modes as needed
         )
  :config
  ;; Optional configuration
  (setq eglot-autoshutdown t)

  ;; Increase timeout for language server initialization (default is 30 seconds)
  (setq eglot-connect-timeout 120)  ;; 2 minutes for gem installation

  ;; Configure code actions to show test run options
  (setq eglot-ignored-server-capabilities nil) ;; Make sure all capabilities are enabled

  ;; (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs '(ruby-mode . ("ruby-lsp"))))

;; Configure flymake and eglot indicators for terminal mode
;; In terminal mode, use ASCII characters to avoid width issues with emoji
;; This works in conjunction with 09_emoji.el which sets emoji width to 2
;; for proper rendering in source files, but we want ASCII in margins
(when (not (display-graphic-p))
  ;; Set flymake margin indicators to simple ASCII
  ;; This prevents layout issues when emoji have width-2 in char-width-table
  (setq flymake-margin-indicators-string
        '((error "!" compilation-error)
          (warning "!" compilation-warning)
          (note "i" compilation-info)))

  ;; Set eglot code action indicator to ASCII
  ;; Default would be 💡 emoji which causes margin spacing issues
  (setq eglot-code-action-indicator ">"))

;; (defun my/find-compile-commands-dir ()
;;   "Locate the directory containing the 'compile_commands.json' file and print it."
;;   (let ((project-root (locate-dominating-file default-directory "compile_commands.json")))
;;     (if project-root
;;         (progn
;;           (message "Found compile_commands.json in directory: %s" project-root)
;;           project-root)
;;       (message "compile_commands.json not found in any parent directory.")
;;       nil)))

;; Assign the directory to a variable and print it.
;; (setq my-compile-commands-dir (my/find-compile-commands-dir))

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

(use-package terraform-mode
  :ensure t)

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
  (setq auto-mode-alist (delete '("Gemfile" . ruby-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("Gemfile\\.lock" . ruby-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("\\.rake\\'" . ruby-ts-mode) auto-mode-alist))
  (setq auto-mode-alist (delete '("\\.rbi\\'" . ruby-ts-mode) auto-mode-alist))

  (global-treesit-auto-mode)

  ;; Make sure we use ruby-mode, not ruby-ts-mode
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rbi\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile.lock" . ruby-mode))
  ;; Use lsp-mode for Ruby and Terraform
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode)))

;; (use-package lsp-mode
;;   :bind
;;   ("M-RET" . lsp-execute-code-action)
;;   ("C-c a" . lsp-avy-lens) ;; For things like Run Test in Rails unit tests
;;   :hook
;;   ((ruby-mode . lsp-deferred)
;;    (terraform-mode . lsp-deferred))
;;   :config
;;   (setq lsp-disabled-clients '(rubocop-ls sorbet-ls semgrep-ls)))
;; Note: there's also an lsp-enabled-clients

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; See also: 06_debug.el
