;;; -*- lexical-binding: t; -*-

;;; common lisp
(setq sly-complete-symbol*-fancy t
      sly-contribs '(sly-fancy sly-indentation sly-autodoc
                     sly-stepper sly-macrostep sly-scratch))

(setq inferior-lisp-program "sbcl")

(with-eval-after-load "sly"
  (sly-setup sly-contribs))

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

(with-eval-after-load "cider"
  (keymap-set cider-repl-mode-map "C-c M-o" #'cider-repl-clear-buffer))

(dolist (hook '(scheme-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook
                lisp-mode-hook common-lisp-mode-hook clojure-mode-hook sly-mrepl-mode-hook
                cider-repl-mode))
  (add-hook hook #'paredit-mode)
  (add-hook hook #'puni-disable-puni-mode))

(with-eval-after-load "paredit"
  (dolist (k '("M-?" "RET"))
    (keymap-unset paredit-mode-map k)))

(with-eval-after-load "geiser"
  (require 'geiser-guile))

(provide 'init-lisp)
