;;; init-lsp.el -*- lexical-binding: t -*-
;;; Commentary: working like ide
;;; Code:

(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :straight t
  :commands company-lsp
  :init
  (setq-default lsp-auto-guess-root nil
		company-transformers nil
		company-lsp-async t
		company-lsp-cache-candidates nil)

  :config
  (use-package yasnippet
    :straight t
    :diminish yas-minor-mode
    :hook (after-init . yas-global-mode)
    :config
    (use-package yasnippet-snippets
      :straight t))

;;  (push 'company-lsp company-backends))
  )

(provide 'init-lsp)

;;; init-lsp.el ends here
