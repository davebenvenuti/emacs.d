(defun env-var-to-list (var-name &optional delimiter)
  "Convert a delimited environment variable specified by VAR-NAME to a list of strings.
An optional DELIMITER can be provided, defaulting to a comma."
  (let ((env-value (getenv var-name))
        (delim (or delimiter ",")))
    (when env-value
      ;; Split the string by the specified delimiter and trim each element
      (mapcar #'string-trim (split-string env-value delim t)))))

(defun env-var-set-p (var)
  "Return non-nil if environment variable VAR is set and not empty."
  (let ((value (getenv var)))
    (and value (not (string-empty-p value)))))

(defun my/setup-gptel ()
  "Setup gptel for hosted deepseek"
  (when (env-var-set-p "GPTEL_DEEPSEEK_API_KEY")
    (setq gptel-model `deepseek-chat
          gptel-backend (gptel-make-deepseek "DeepSeek"
                                             :stream t
                                             :key (getenv "GPTEL_DEEPSEEK_API_KEY")
                                             ))))

;; gptel notes
;;
;;  - C-c <ret> sends the message in the `gptel` chat buffer
(use-package gptel
  :straight t
  :config
  (my/setup-gptel))

;; old
;; (use-package gptel
;;   :straight t
;;   :config
;;   (setq gptel-model (intern (getenv "GPTEL_DEFAULT_MODEL"))
;;       gptel-backend
;;       (gptel-make-openai (or (getenv "GPTEL_NAME") "OpenAI")
;;         :host (getenv "GPTEL_API_HOST")
;;         :endpoint (getenv "GPTEL_API_ENDPOINT")
;;         :stream t
;;         :key (getenv "GPTEL_API_KEY")
;;         :models (env-var-to-list "GPTEL_MODELS"))
;;       gptel-max-tokens (string-to-number (or (getenv "GPTEL_MAX_TOKENS") "16384"))))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config
  (setq copilot-indent-offset-warning-disable t)
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "C-c n") 'copilot-next-completion)
  ;; https://github.com/copilot-emacs/copilot.el/issues/312
  (add-to-list 'copilot-indentation-alist '(prog-mode tab-width))
  (add-to-list 'copilot-indentation-alist '(org-mode tab-width)))
