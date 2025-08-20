;;; -*- lexical-binding: t; -*-

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(haskell-interactive-mode . normal)))

(provide 'init-haskell)

