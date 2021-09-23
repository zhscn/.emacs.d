;;; -*- lexical-binding: t; -*-

(leaf sly-asdf
  :straight t
  :after (sly company))

(leaf sly-quicklisp
  :straight t
  :after sly)

(leaf sly
  :straight t
  :init
  (setq sly-complete-symbol*-fancy t)
  (setq sly-contribs '(sly-fancy
                       sly-indentation
                       sly-autodoc
                       sly-sbcl-exts
                       sly-scratch))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal))
  :config
  (sly-setup '(sly-fancy sly-asdf sly-quicklisp)))

(leaf common-lisp-snippets
  :straight t
  :after yasnippet)

(provide 'init-cl)
