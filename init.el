;;; -*- lexical-binding: t; -*-
(setq modus-themes-slanted-constructs t)

(load-theme 'modus-vivendi t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'init-builtin)
(require 'init-package)

(require 'init-complete)
(require 'init-theme)

(require 'init-rime)
(require 'init-telega)

(require 'init-meow)

(require 'init-cc)
(require 'init-lisp)
(require 'init-coq)
(require 'init-haskell)
(require 'init-company)
(require 'init-lsp)

(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
