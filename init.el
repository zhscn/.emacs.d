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

(defun load-lsp-bridge ()
  (add-to-list 'load-path (expand-file-name "lsp-bridge" user-emacs-directory))
  (require 'lsp-bridge)
  (setq lsp-bridge-c-lsp-server "ccls")
  (add-hook 'after-init-hook #'global-lsp-bridge-mode)
  (keymap-set global-map "M-." #'lsp-bridge-find-def)
  (keymap-set global-map "M-," #'lsp-bridge-find-def-return)
  (keymap-set global-map "M-?" #'lsp-bridge-find-references)
  (keymap-set global-map "M-'" #'lsp-bridge-code-action)
  (keymap-set global-map "M-[" #'lsp-bridge-code-format))

(defun load-lsp ()
  (require 'init-company)
  (require 'init-lsp))

(defvar use-lsp-bridge nil)

(if use-lsp-bridge
    (load-lsp-bridge)
  (load-lsp))

(when (file-exists-p custom-file) (load custom-file))
(put 'dired-find-alternate-file 'disabled nil)
