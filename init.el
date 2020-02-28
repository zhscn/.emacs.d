;;; init.el -*- lexical-binding: t -*-
;;; Commentary: Emacs Configuration
;;; Code:

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1080 5))

(setq gc-cons-threshold (* 500 1024 1024))
(setq gc-cons-percentage 0.5)
(setq load-prefer-newer noninteractive)

(defun update-load-path (&rest _)
  "Update `load-path'."
  (push (expand-file-name "lisp" user-emacs-directory) load-path))
(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(let (file-name-handler-alist)
  (setq user-emacs-directory (file-name-directory load-file-name)))

(require 'init-base)
(require 'init-benchmarking)
(require 'init-package)
(require 'init-theme)

(require 'init-evil)
(require 'init-ivy)
(require 'init-company)
(require 'init-lsp)
(require 'init-flycheck)

(require 'init-cc)

(require 'init-funcs)
(require 'init-dired)
(require 'init-parens)

(provide 'init)

;;; init.el ends here
