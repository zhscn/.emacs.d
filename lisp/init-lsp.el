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
    company-lsp-cache-candidates t)

  :config
  (use-package yasnippet
    :straight t
    ; :diminish yas-minor-mode
    :hook (after-init . yas-global-mode)
    :config
    (yas-minor-mode)
    (yas-reload-all)
    (use-package yasnippet-snippets
      :straight t
      :after yasnippet)))

(use-package dap-mode
  :straight t
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle))))

(provide 'init-lsp)

;;; init-lsp.el ends here
