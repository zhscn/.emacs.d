;;; -*- lexical-binding: t; -*-

(leaf geiser
  :straight t
  :require t
  :config
  (leaf geiser-chez
    :straight t
    :require t))

(leaf paredit
  :straight t
  :require t
  :hook scheme-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook lisp-mode-hook racket-mode-hook)

(leaf racket-mode
  :straight t
  :mode "\\.rkt\\'")

; (setq tab-always-indent 'complete)
(provide 'init-scheme)
