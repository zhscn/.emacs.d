;;; -*- lexical-binding: t -*-
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(add-hook 'c-mode-hook #'tree-sitter-mode)
(add-hook 'c-mode-hook #'tree-sitter-hl-mode)
(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-hl-mode)

(provide 'init-tree-sitter)
