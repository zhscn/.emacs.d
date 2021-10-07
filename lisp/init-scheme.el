;;; -*- lexical-binding: t; -*-
(straight-use-package 'geiser)
(straight-use-package 'geiser-chez)
(straight-use-package 'paredit)

(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)

(provide 'init-scheme)
