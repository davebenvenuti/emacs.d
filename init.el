;; straight.el (use-package extension)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun load-el-if-exists (relative-path)
  "Load the Emacs Lisp file FILENAME if it exists."
  (let ((full-path (expand-file-name relative-path user-emacs-directory)))
    (when (file-exists-p full-path)
      (load full-path))))

(load-el-if-exists "./env.private.el")
(load-el-if-exists "./init.private.el")

(dolist (f '("./ai" "./keys" "./git" "./lsp" "./popups" "./misc"))
  (load (expand-file-name f user-emacs-directory)))
