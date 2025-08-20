;;; -*- lexical-binding: t -*-
(straight-use-package 'rg)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(autoload #'cmake-mode "cmake-mode" nil t)

(setq lsp-bridge-c-lsp-server "ccls")
(with-eval-after-load 'cc-mode
  (keymap-unset c-mode-map "C-c .")
  (keymap-unset c++-mode-map "C-c ."))

(provide 'init-cc)
