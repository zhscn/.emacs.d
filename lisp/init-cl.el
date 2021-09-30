;;; -*- lexical-binding: t; -*-



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

  :config
  (leaf sly-asdf :straight t)
  (leaf sly-quicklisp :straight t)

  (sly-setup '(sly-fancy sly-asdf sly-quicklisp))
  (with-eval-after-load "meow"
      (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal))))

(leaf common-lisp-snippets
  :straight t
  :after yasnippet)

(provide 'init-cl)
