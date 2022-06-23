;;; -*- lexical-binding: t -*-
(straight-use-package '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*")))

(straight-use-package 'yasnippet)
(straight-use-package 'markdown-mode)
(straight-use-package 'smartparens)
(straight-use-package 'posframe)

(setq acm-candidate-match-function 'orderless-flex)

(yas-global-mode 1)

(add-hook 'after-init-hook #'global-lsp-bridge-mode)
(smartparens-global-mode +1)
(show-smartparens-global-mode +1)
(sp-with-modes
    '(c++-mode objc-mode c-mode)
  (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))
(provide 'init-lsp)
