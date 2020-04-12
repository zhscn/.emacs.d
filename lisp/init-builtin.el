;;; init-builtin.el -*- lexical-binding: t -*-
;;; Commentary: some builtin configurations
;;; Code:

(use-package recentf
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

(setq whitespace-global-modes '(not makefile-mode))
(use-package whitespace
  :ensure nil
  :hook ((after-init . global-whitespace-mode)
         (before-save . (lambda () (progn
                                       ;; (untabify (point-min) (point-max))
                                       (whitespace-cleanup)))))
  :config
  (setq-default indent-tabs-mode nil)
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))
  (face-spec-set 'whitespace-line
                 '((((background light))
            :background "#d8d8d8" :foreground unspecified
            :underline t :weight unspecified)
           (t
            :background "#404040" :foreground unspecified
            :underline t :weight unspecified)))

  (face-spec-set 'whitespace-space-before-tab
         '((((background light))
            :background "#d8d8d8" :foreground "#de4da1")
           (t
            :inherit warning
            :background "#404040" :foreground "#ee6aa7")))
  (setq
   whitespace-line-column 80
   whitespace-style
   '(face
     empty
     lines-tail
     space-before-tab
     trailing
     tabs
     tab-mark)))

(provide 'init-builtin)

;;; init-builtin.el ends here
