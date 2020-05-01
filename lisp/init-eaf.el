;;; init-eaf.el -*- lexical-binding: t -*-
;;; Commentary: emacs application framework 
;;; Code:

(use-package eaf
  :load-path "~/pkg/emacs-application-framework"
  :init
  (require 'eaf)
  :after evil
  :custom
  (eaf-find-alternate-file-in-dired t)
  :config
  (evil-set-initial-state 'eaf-mode 'emacs)
  (evil-set-initial-state 'eaf-edit-mode 'emacs)
  (evil-set-initial-state 'eaf-interleave-mode 'emacs)
  (evil-set-initial-state 'eaf-pdf-outline-mode 'emacs)
  (evil-set-initial-state 'eaf-edit-buffer-switch-ot-org-mode 'emacs)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding))

(provide 'init-eaf)

;;; init-eaf.el ends here
