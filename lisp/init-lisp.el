;;; -*- lexical-binding: t; -*-

;;; scheme
(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(geiser-repl-mode . normal)))

;;; common lisp
(setq sly-complete-symbol*-fancy t
      sly-contribs '(sly-fancy
                     sly-indentation
                     sly-autodoc
                     sly-stepper
                     sly-macrostep
                     sly-scratch))

(setq inferior-lisp-program "ros -Q run")

(with-eval-after-load "sly"
  (sly-setup '(sly-fancy sly-asdf sly-quicklisp sly-macrostep)))

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(sly-mrepl-mode . normal)))

;;; clojure
(setq cider-font-lock-dynamically nil
      cider-font-lock-reader-conditionals t
      cider-use-fringe-indicators t
      cider-prompt-for-symbol nil
      cider-save-file-on-load t
      cider-enhanced-cljs-completion-p t
      cider-offer-to-open-cljs-app-in-browser nil)

(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'common-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

(provide 'init-lisp)
