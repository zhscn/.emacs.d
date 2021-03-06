;;; init-ivy.el -*- lexical-binding: t -*-
;;; Commentary: to be honest, i don't know about its uses
;;; Code:

(use-package counsel
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :bind (:map ivy-minibuffer-map
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

  ; (use-package fuz
  ;   :init
  ;   (require 'fuz)
  ;   (unless (require 'fuz-core nil t)
  ;     (fuz-build-and-load-dymod))
  ;   :config
  ;   (use-package ivy-fuz
  ;     :after ivy
  ;     :custom
  ;     (ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
  ;     (ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy)))
  ;     :config
  ;     (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn))))

  (use-package amx
    :ensure t
    :init (setq amx-history-length 20))

  (use-package ivy-hydra
    :commands ivy-hydra-read-action
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  (use-package counsel-projectile
    :hook (counsel-mode . counsel-projectile-mode)
    :init
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    (counsel-projectile-mode 1))

  (use-package ivy-yasnippet
    :commands ivy-yasnippet--preview
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

  (use-package ivy-xref
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)))

(use-package ivy-rich
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
