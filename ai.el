;; aider

;; 1)
;;   python -m pip install aider-install
;;   aider-install
;; 2) https://github.com/tninja/aider.el

(setq aider-args '("--no-gitignore"))

;; Also needs the following environment variables set in env.private.el:

;;   AIDER_MODEL
;;   AIDER_4O (maybe optional?)
;;   AIDER_OPENAI_API_BASE
;;   AIDER_OPENAI_API_KEY
;;   AIDER_AUTO_COMMITS
