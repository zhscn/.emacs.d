;;; -*- lexical-binding: t; -*-
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "theme" user-emacs-directory))

(require 'init-site-lisp)
(require 'init-builtin)
(require 'init-package)

(require 'init-complete)
(require 'init-theme)

(require 'init-rime)
(require 'init-org)
(require 'init-window)
(require 'init-meow)
(require 'init-tramp)

(require 'init-lisp)
(require 'init-coq)
(require 'init-haskell)
(require 'init-zig)

;; (require 'init-lsp-bridge)
(require 'init-company)
(require 'init-cpp)

(require 'init-tree-sitter)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
