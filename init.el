(menu-bar-mode -1)
(global-display-line-numbers-mode t)

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
   '(popper use-package editorconfig protobuf-mode ag eglot git-gutter git-gutter-fringe))
 '(warning-suppress-log-types
   '((copilot copilot-no-mode-indent)
     (copilot copilot-no-mode-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GEM_HOME")
  (exec-path-from-shell-copy-env "GEM_PATH")
  (exec-path-from-shell-copy-env "RUBY_VERSION"))

;; M-? allows you to navigate references etc
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  (add-to-list 'eglot-server-programs
               `(c++-mode . ("clangd" "--compile-commands-dir=/tmp"))))

;; Prevent eldoc (and eglot) from resizing the echo area on hover
(setq eldoc-echo-area-use-multiline-p nil)

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-pop-up-window)
               (window-width . 60))) ;; Set the width to x columns

(delete-selection-mode 1)

(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

(require 'git-gutter)

;; If you enable global minor mode
;;(global-git-gutter-mode t)
(global-git-gutter-mode +1)

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-'"   . popper-toggle) ;; TODO doesn't work
         ("M-'"   . popper-cycle)
         ("C-M-'" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
	  "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(defun load-el-if-exists (relative-path)
  "Load the Emacs Lisp file FILENAME if it exists."
  (let ((full-path (expand-file-name relative-path user-emacs-directory)))
    (when (file-exists-p full-path)
      (load full-path))))

(load-el-if-exists "./init.private.el")

