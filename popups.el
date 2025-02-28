;; Prevent eldoc (and eglot) from resizing the echo area on hover
(setq eldoc-echo-area-use-multiline-p nil)

(add-to-list 'display-buffer-alist
             '("\\*eldoc\\*"
               (display-buffer-pop-up-window)
               (window-width . 60))) ;; Set the width to x columns

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
