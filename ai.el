;; aider

;; 1)
;;   python -m pip install aider-install
;;   aider-install
;; 2) https://github.com/MatthewZMD/aidermacs

(use-package aidermacs
  :straight (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (setq aidermacs-default-model "sonnet")
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu)
  (setq aidermacs-auto-commits 0)
  (setq aidermacs-use-architect-mode 0))

;; Also needs the following environment variables set in env.private.el:

;;   AIDER_MODEL
;;   AIDER_4O (maybe optional?)
;;   AIDER_OPENAI_API_BASE
;;   AIDER_OPENAI_API_KEY
;;   AIDER_AUTO_COMMITS
