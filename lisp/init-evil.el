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
        evil-normal-state-cursor '(box "grey60")))

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
  (defalias 'unmap! #'general-unbind))

(use-package which-key
  :straight t
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (setq-hook! 'which-key-init-buffer-hook line-spacing 3)

  (which-key-add-key-based-replacements doom-leader-key "<leader>")
  (which-key-add-key-based-replacements doom-localleader-key "<localleader>")

  (which-key-mode +1))

(provide 'init-evil)

;;; init-evil.el ends here
