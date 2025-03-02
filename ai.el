;; aider

;; 1)
;;   python -m pip install aider-install
;;   aider-install
;; 2) https://github.com/MatthewZMD/aidermacs

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "deepseek/deepseek-chat")
  (setq aidermacs-editor-model "deepseek/deepseek-coder")
  (setq aidermacs-architect-model "deepseek/deepseek-coder")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setq aidermacs-auto-commits nil)
  (setq aidermacs-use-architect-mode t)
  (setq aidermacs-extra-args '("--no-gitignore")))


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))
