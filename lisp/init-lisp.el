;;; -*- lexical-binding: t; -*-
(straight-use-package 'paredit)
(straight-use-package 'sly)
(straight-use-package 'sly-asdf)
(straight-use-package 'sly-quicklisp)
(straight-use-package '(sly-stepper :type git :host github :repo "joaotavora/sly-stepper"
                                    :files (:defaults "*.el" "*.lisp" "*.asd")))
(straight-use-package 'common-lisp-snippets)

(straight-use-package 'geiser)
(straight-use-package 'geiser-guile)

(straight-use-package 'clojure-mode)
(straight-use-package 'cider)

;;; scheme
(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(geiser-repl-mode . normal)))

;;; common lisp
(setq sly-complete-symbol*-fancy t
      sly-contribs '(sly-fancy
                     sly-indentation
                     sly-autodoc
                     sly-stepper
                     sly-scratch))

(setq inferior-lisp-program "ros -Q run")

(with-eval-after-load "sly"
  (sly-setup '(sly-fancy sly-asdf sly-quicklisp)))

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
  (defun +clojure-describe-spec ()
    (interactive)
    (when-let* ((code (thing-at-point 'symbol))
                (dict (cider-nrepl-sync-request:eval
                       code
                       (--find (eq (cider-connection-type-for-buffer)
                                   (cider-connection-type-for-buffer it))
                               (cider-connections))
                       (cider-ns-from-form (cider-ns-form))))
                (spec (-last-item dict)))
      (cider-browse-spec spec))))

(add-hook 'scheme-mode-hook #'paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'lisp-mode-hook #'paredit-mode)
(add-hook 'common-lisp-mode-hook #'paredit-mode)
(add-hook 'clojure-mode-hook #'paredit-mode)

(provide 'init-lisp)
