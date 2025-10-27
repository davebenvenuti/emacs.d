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

;; ==============================================================================
;; FIX: Auto-adjust margin width for eglot code action indicators (terminal mode)
;; ==============================================================================
;;
;; PROBLEM:
;; When running Emacs in terminal mode with emoji configured to display width 2
;; (via char-width-table in 09_emoji.el), eglot's code action indicator (💡)
;; causes visual alignment glitches. The lightbulb emoji is 2 characters wide,
;; but eglot doesn't automatically resize the margin to accommodate it (unlike
;; flymake which has auto-resize logic). This causes the indicator to overflow,
;; pushing line numbers and code text to the right.
;;
;; ROOT CAUSE:
;; - 09_emoji.el sets emoji to width-2 in char-width-table (correct for buffer content)
;; - string-width() correctly returns 2 for the lightbulb emoji
;; - eglot displays the indicator in the margin (when 'margin is in eglot-code-action-indications)
;; - eglot does NOT auto-resize margins based on indicator width
;; - Default margin width is 1, causing 2-char emoji to overflow
;;
;; SOLUTION:
;; Hook into eglot-managed-mode-hook to automatically adjust left-margin-width
;; based on the calculated string-width of the indicator. This mimics what
;; flymake does internally with flymake-autoresize-margins.
;;
;; REPRODUCING FOR OTHER MODULES:
;; If another package displays wide characters (emoji, CJK, etc.) in margins
;; without auto-resizing, follow this pattern:
;; 1. Identify the variable holding the indicator character(s)
;; 2. Calculate width using (string-width indicator)
;; 3. Set left-margin-width (or right-margin-width) to at least that width
;; 4. Call (set-window-buffer (selected-window) (current-buffer)) to refresh
;; 5. Hook into the appropriate mode hook to run automatically
;;
(when (not (display-graphic-p))
  (defun my/auto-adjust-eglot-margin ()
    "Automatically set margin width to accommodate emoji code action indicator.
Eglot doesn't auto-resize margins like flymake does, so we do it manually."
    (when (and eglot--managed-mode
               (memq 'margin eglot-code-action-indications))
      (let* ((indicator eglot-code-action-indicator)
             (indicator-width (string-width indicator))
             (required-width (max indicator-width (or left-margin-width 0))))
        (unless (and left-margin-width (>= left-margin-width required-width))
          (setq left-margin-width required-width)
          ;; Force window refresh to apply the new margin width
          (set-window-buffer (selected-window) (current-buffer))))))

  (add-hook 'eglot-managed-mode-hook #'my/auto-adjust-eglot-margin))

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
