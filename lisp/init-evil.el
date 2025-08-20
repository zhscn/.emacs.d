;;; init-evil.el -*- lexical-binding: t -*-
;;; Commentary: vim key binding
;;; Code:

(use-package evil
  :straight t
  :ensure t
  :hook (after-init . evil-mode)
  :init (setq evil-want-keybinding nil)
  :config
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq evil-emacs-state-cursor '(box "SteelBlue")
        evil-normal-state-cursor '(box "grey60"))

  (defun change-line-number-relative()
     (setq display-line-numbers 'visual))
  
  (defun change-line-number-abs()
     (setq display-line-numbers 'abslute))

  (add-hook 'evil-insert-state-entry-hook 'change-line-number-abs)
  (add-hook 'evil-normal-state-entry-hook 'change-line-number-relative)
  (add-hook 'evil-motion-state-entry-hook 'change-line-number-relative))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(provide 'init-evil)

;;; init-evil.el ends here
