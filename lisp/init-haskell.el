;;; -*- lexical-binding: t; -*-
(straight-use-package 'haskell-mode)
(straight-use-package 'dante)

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(haskell-interactive-mode . normal)))

(provide 'init-haskell)

