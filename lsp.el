(provide 'lsp-init)

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
(with-eval-after-load 'eglot
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (let ((root (locate-dominating-file
                           default-directory
                           "compile_commands.json")))
                (when root
                  (setq default-directory root))))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs
               `(c++-mode 2. ("clangd" (concat "--compile-commands-dir=" my-compile-commands-dir) "--query-driver=/**/*"))))
