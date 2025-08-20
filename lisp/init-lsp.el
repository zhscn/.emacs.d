;; -*- lexical-binding: t; -*-

(setq lsp-enable-symbol-highlighting nil
      lsp-headerline-breadcrumb-enable nil
      lsp-enable-on-type-formatting nil
      lsp-lens-enable nil
      lsp-enable-indentation nil)

(with-eval-after-load "cc-mode"
  (keymap-set c-mode-base-map "RET" #'c-context-line-break)
  (keymap-unset c++-mode-map ":")
  (keymap-unset c++-mode-map ",")
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp))

(with-eval-after-load "lsp-mode"
  (require 'ccls))

(with-eval-after-load "ccls"
  (defun ccls-inheritance-hierarchy-derived () (interactive) (ccls-inheritance-hierarchy t))
  (defun ccls-inheritance-hierarchy-base () (interactive) (ccls-inheritance-hierarchy nil))

  (keymap-set lsp-mode-map "C-c v c" #'ccls-call-hierarchy)
  (keymap-set lsp-mode-map "C-c v d" #'ccls-inheritance-hierarchy-derived)
  (keymap-set lsp-mode-map "C-c v b" #'ccls-inheritance-hierarchy-base))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'lsp)
(add-hook 'rust-ts-mode-hook #'flycheck-mode)

(provide 'init-lsp)
