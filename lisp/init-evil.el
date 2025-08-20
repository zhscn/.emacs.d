;;; init-evil.el -*- lexical-binding: t -*-
;;; Commentary: vim key binding
;;; Code:

(use-package evil
  :straight t
  :ensure t
  :hook ((after-init . evil-mode)
         (evil-insert-state-entry . (lambda () (setq display-line-numbers 'abslute)))
         (evil-normal-state-entry . (lambda () (setq display-line-numbers 'visual))))
  :init (setq evil-want-keybinding nil
              evil-want-C-u-scroll t)
  :config
  (setq evil-emacs-state-cursor '(box "SteelBlue")
        evil-normal-state-cursor '(box "grey60"))

  (require 'key-chord)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(use-package general
  :straight t
  :init
  ;; Convenience aliases
  (defalias 'define-key! #'general-def)
  (defalias 'unmap! #'general-unbind)
  (defalias 'def-key #'general-def)
  :config

  (general-create-definer leader
  ;; :prefix my-leader
  :prefix "SPC")

  (general-create-definer local-leader
  ;; :prefix my-local-leader
  :prefix "SPC m"))

(provide 'init-evil)

;;; init-evil.el ends here
