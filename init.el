(require 'package)
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
;;        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(copilot-chat rg popper use-package editorconfig protobuf-mode ag eglot git-gutter git-gutter-fringe))
 '(warning-suppress-log-types
   '((copilot copilot-no-mode-indent)
     (copilot copilot-no-mode-indent))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun load-el-if-exists (relative-path)
  "Load the Emacs Lisp file FILENAME if it exists."
  (let ((full-path (expand-file-name relative-path user-emacs-directory)))
    (when (file-exists-p full-path)
      (load full-path))))

(dolist (f '("./keys" "./git" "./lsp" "./popups" "./misc"))
  (load (expand-file-name f user-emacs-directory)))

(load-el-if-exists "./env.private.el")
(load-el-if-exists "./init.private.el")
