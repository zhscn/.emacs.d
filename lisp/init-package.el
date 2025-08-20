;;; init-package.el -*- lexical-binding: t -*-
;;; Commentary: package management configurations
;;; Code:

(require 'init-base)
(require 'init-benchmarking)

; (setq straight-recipes-gnu-elpa-use-mirror t
;       straight-vc-git-default-clone-depth 1)

; (defvar bootstrap-version)
; (let ((bootstrap-file
;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;       (bootstrap-version 5))
;   (unless (file-exists-p bootstrap-file)
;     (with-current-buffer
;         (url-retrieve-synchronously
;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;          'silent 'inhibit-cookies)
;       (goto-char (point-max))
;       (eval-print-last-sexp)))
;   (load bootstrap-file nil 'nomessage))

(require 'package)

(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package diminish)
(use-package bind-key)
(use-package gnu-elpa-keyring-update)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(require 'init-builtin)

(provide 'init-package)

;;; init-package.el ends here
