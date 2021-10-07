;;; -*- lexical-binding: t; -*-
(straight-use-package 'sly)
(straight-use-package 'sly-asdf)
(straight-use-package 'sly-quicklisp)
(straight-use-package 'common-lisp-snippets)

(setq sly-complete-symbol*-fancy t)
(setq sly-contribs '(sly-fancy
                     sly-indentation
                     sly-autodoc
                     sly-sbcl-exts
                     sly-scratch))
(setq inferior-lisp-program "/usr/bin/sbcl")

(require 'sly)
(sly-setup '(sly-fancy sly-asdf sly-quicklisp))
(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal)))

(provide 'init-cl)
