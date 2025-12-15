;;; -*- lexical-binding: t; -*-

(dolist (dir '("lisp" "site-lisp" "agda2-mode"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'init-builtin)
(require 'init-package)

(require 'init-complete)
(require 'init-theme)

(require 'init-rime)
(require 'init-telega)

(require 'init-meow)

(require 'init-agda)
(require 'init-cc)
(require 'init-lisp)
(require 'init-coq)

(require 'init-lsp)

(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)

(setq modus-themes-slanted-constructs t)
(load-theme 'modus-vivendi t)
