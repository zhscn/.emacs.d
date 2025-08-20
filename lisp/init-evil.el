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
  ;; (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq evil-emacs-state-cursor '(box "SteelBlue")
	evil-normal-state-cursor '(box "grey60")))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (setq evil-collection-company-use-tng nil)
  (evil-collection-init))

(provide 'init-evil)

;;; init-evil.el ends here
