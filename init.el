;;; -*- lexical-binding: t; -*-

(require 'init-complete)
(require 'init-company)
(require 'init-theme)

(require 'init-rime)
(require 'init-org)
(require 'init-telega)
(require 'init-magit)
(require 'init-window)
(require 'init-meow)

(require 'init-lisp)
(require 'init-coq)
(require 'init-cpp)

;(require 'init-koka)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
