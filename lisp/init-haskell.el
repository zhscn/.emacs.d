;;; init-haskell.el -*- lexical-binding: t -*-
;;; Commentary: Haskell
;;; Code:

(use-package haskell-mode
  :ensure t)

(use-package dante
  :ensure t
  :hook (haskell-mode . dante-mode))

(require 'hs-lint)

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :config
  (push 'dante-company company-backends)
  (setq lsp-haskell-process-path-hie "ghcide")
  (setq lsp-haskell-process-args-hie '()))


(provide 'init-haskell)

;;; init-haskell.el ends here
