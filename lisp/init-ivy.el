;;; init-ivy.el -*- lexical-binding: t -*-
;;; Commentary: to be honest, i don't know about its uses
;;; Code:

(use-package counsel
  :straight t
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (("C-s" . swiper-isearch)
         :map ivy-minibuffer-map
              ([escape] . 'minibuffer-keyboard-quit))
  :init
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil)

  (setq swiper-action-recenter t)

  :config
  (setq ivy-initial-inputs-alist nil)
  (with-no-warnings
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read)))

  (use-package amx
    :straight t
    :init (setq amx-history-length 20))

  (use-package ivy-hydra
    :straight t
    :commands ivy-hydra-read-action
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  (use-package counsel-projectile
    :straight t
    :init
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    (counsel-projectile-mode 1))

  (use-package counsel-projectile
    :straight t
    :hook (counsel-mode . counsel-projectile-mode)
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  (use-package ivy-yasnippet
    :straight t
    :commands ivy-yasnippet--preview
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

  (use-package ivy-xref
    :straight t
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))

(use-package ivy-rich
  :straight t
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  (setq ivy-rich-parse-remote-buffer nil)

  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1))))

(provide 'init-ivy)

;;; init-ivy.el ends here
