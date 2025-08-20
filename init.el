;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "theme" user-emacs-directory))
(require 'init-site-lisp)

(require 'init-package)
(require 'init-builtin)
(require 'init-complete)
(require 'init-theme)
(require 'init-company)
(require 'init-meow)

;(require 'init-eaf)
(require 'init-rime)
(require 'init-org)
(require 'init-telega)
(require 'init-magit)

(require 'init-coq)
(require 'init-cpp)
(require 'init-scheme)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
