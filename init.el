;;; init.el -*- lexical-binding: t -*-
;;; Commentary: Emacs Configuration
;;; Code:

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(setq url-gateway-method 'socks)
(setq socks-server '("Default server" "127.0.0.1" 1080 5))
(setq user-emacs-directory (file-name-directory load-file-name))
(setq gc-cons-percentage 0.5)
(setq load-prefer-newer noninteractive)

(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'init-package)
(require 'init-theme)

(require 'init-evil)
(require 'init-ivy)
(require 'init-company)
(require 'init-lsp)
(require 'init-flycheck)

(require 'init-cc)
(require 'init-rust)

(require 'init-term)
(require 'init-utils)
(require 'init-funcs)
(require 'init-dired)
(require 'init-parens)

(provide 'init)

;;; init.el ends here
