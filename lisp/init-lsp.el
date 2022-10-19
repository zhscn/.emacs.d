;;; -*- lexical-binding: t -*-
(straight-use-package '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*")))

(straight-use-package 'popon)
(straight-use-package 'markdown-mode)
(straight-use-package 'smartparens)
(straight-use-package 'posframe)

(setq acm-candidate-match-function 'orderless-flex)

(yas-global-mode 1)

(global-lsp-bridge-mode)

(unless (display-graphic-p)
  (add-to-list 'load-path (expand-file-name "acm-terminal" user-emacs-directory))
  (with-eval-after-load 'acm
    (require 'acm-terminal)))

(smartparens-global-mode +1)
(show-smartparens-global-mode +1)
(sp-with-modes
    '(c++-mode objc-mode c-mode)
  (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-mode . motion))
  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-switch-to-view-mode . motion)))

(provide 'init-lsp)
