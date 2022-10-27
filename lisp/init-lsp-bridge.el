;;; -*- lexical-binding: t -*-
(straight-use-package '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge" :files (:defaults "*")))

(straight-use-package 'popon)
(straight-use-package 'markdown-mode)
(straight-use-package 'posframe)

(setq acm-candidate-match-function 'orderless-flex)

(yas-global-mode 1)

(global-lsp-bridge-mode)

(keymap-set global-map "C-c ." #'lsp-bridge-find-def)
(keymap-set global-map "C-c ," #'lsp-bridge-find-def-return)
(keymap-set global-map "C-c >" #'lsp-bridge-find-def-other-window)
(keymap-set global-map "C-c ?" #'lsp-bridge-find-references)

(unless (display-graphic-p)
  (add-to-list 'load-path (expand-file-name "acm-terminal" user-emacs-directory))
  (with-eval-after-load 'acm
    (require 'acm-terminal)))

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-mode . motion))
  (add-to-list 'meow-mode-state-list '(lsp-bridge-ref-switch-to-view-mode . motion)))

(setq lsp-bridge-c-lsp-server "ccls")
(with-eval-after-load 'cc-mode
  (keymap-unset c-mode-map "C-c .")
  (keymap-unset c++-mode-map "C-c ."))

(provide 'init-lsp-bridge)