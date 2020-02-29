;;; init-package.el -*- lexical-binding: t -*-
;;; Commentary: package management configurations
;;; Code:

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

; (setq package-archives '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;                          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
;                          ("org"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/org/")))

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

(use-package paradox
  :straight t
  :defer t
  :init
  (setq paradox-execute-asynchronously t
  paradox-github-token t
  paradox-display-star-count nil)

  (defun my-paradox-enable (&rest _)
    "Enable paradox, overriding the default package-menu."
    (paradox-enable))
  (advice-add #'list-packages :before #'my-paradox-enable)

  :config
  (when (fboundp 'page-break-lines-mode)
    (add-hook 'paradox-after-execute-functions
  (lambda (&rest _)
    (let ((buf (get-buffer-create "*Paradox Report*"))
    (inhibit-read-only t))
      (with-current-buffer buf
  (page-break-lines-mode 1))))
  t)))

(use-package which-key
  :straight t
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode))

(use-package auto-package-update
  :straight t
  :defer t
  :init
  (setq auto-package-update-delete-old-versions t
  auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))

(use-package recentf
  :straight t
  :ensure t
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 300
  recentf-exclude
  '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
    "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
    "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
    "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
    (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(defun update-packages (&optional sync)
  "Refresh package contents and update all packages.
If SYNC is non-nil, the updating process is synchronous."
  (interactive)
  (message "Updating packages...")
  (if (and (not sync)
     (require 'async nil t))
      (async-start
       `(lambda ()
    ,(async-inject-variables "\\`\\(load-path\\)\\'")
    (upgrade-packages)
    (with-current-buffer auto-package-update-buffer-name
      (buffer-string)))
       (lambda (result)
   (message "%s" result)
   (message "Updating packages...done")))
    (progn
      (upgrade-packages)
      (message "Updating packages...done"))))

(use-package ediff
  :ensure nil
  :config (setq ediff-split-window-function 'split-window-horizontally))

(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(provide 'init-package)

;;; init-package.el ends here
