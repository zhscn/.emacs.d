;;; init-evil.el -*- lexical-binding: t -*-
;;; Commentary: vim key binding
;;; Code:

(setq evil-normal-state-tag " N")
(setq evil-insert-state-tag " I")
(setq evil-visual-state-tag " V")
(setq evil-emacs-state-tag " E")
(setq evil-motion-state-tag " M")
(setq evil-replace-state-tag " R")
(setq evil-operator-state-tag " O")

(use-package evil
  :ensure t
  :hook ((after-init . evil-mode))
         ; (evil-insert-state-entry . (lambda () (setq display-line-numbers 'abslute)))
         ; (evil-normal-state-entry . (lambda () (setq display-line-numbers 'visual))))
  :init (setq evil-want-keybinding nil
              evil-want-C-u-scroll t)
  :config
  (setq evil-emacs-state-cursor '(box "SteelBlue")
        evil-normal-state-cursor '(box "grey60")))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package general
  :init
  (defalias 'define-key! #'general-def)
  (defalias 'unmap! #'general-unbind)
  (defalias 'def-key #'general-def)
  :config

  (general-create-definer leader
  ;; :prefix leader
  :prefix "SPC")

  (general-create-definer local-leader
  ;; :prefix local-leader
  :prefix "SPC m"))

(leader 'normal
  "." 'counsel-find-file
  "SPC" 'projectile-find-file
  "fr" 'counsel-recentf
  "fs" 'save-buffer
  "bb" 'ivy-switch-buffer
  "bk" 'kill-current-buffer
  "bK" 'kill-buffer
  "ww" 'ace-window
  "wj" 'windmove-down
  "wk" 'windmove-up
  "wh" 'windmove-left
  "wl" 'windmove-right)

(def-key 'normal
  "H" 'evil-first-non-blank
  "L" 'evil-end-of-line)

(provide 'init-evil)

;;; init-evil.el ends here
