;;; init-package.el -*- lexical-binding: t -*-
;;; Commentary: package management configurations
;;; Code:

(setq straight-recipes-gnu-elpa-use-mirror t
      straight-repository-branch "develop"
      straight-vc-git-default-clone-depth 1)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
   'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; (require 'package)

(setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

; (package-initialize)

; (unless (package-installed-p 'use-package)
;   (package-refresh-contents)
;   (package-install 'use-package))

(straight-use-package 'use-package)
(straight-use-package 'diminish)
(straight-use-package 'bind-key)
(straight-use-package 'gnu-elpa-keyring-update)

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

; (use-package benchmark-init
;   :straight t
;   :ensure t
;   :hook ((after-init . benchmark-init/deactivate)))

(use-package which-key
  :straight t
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode))

(use-package recentf
  :straight t
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
  recentf-exclude
  '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
    "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
    "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
    "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
    (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(provide 'init-package)

;;; init-package.el ends here
