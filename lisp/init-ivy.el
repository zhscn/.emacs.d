;;; init-ivy.el -*- lexical-binding: t -*-
;;; Commentary: to be honest, i don't know about its uses
;;; Code:

(use-package counsel
  :straight t
  :diminish ivy-mode counsel-mode
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq enable-recursive-minibuffers t)

  (setq ivy-use-selectable-prompt t
        ivy-use-virtual-buffers t
        ivy-height 15
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil)

  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never '%s' %s"))
  
  :config
  (setq ivy-initial-inputs-alist nil)
  (with-no-warnings
    ;; Integration with `projectile'
    (with-eval-after-load 'projectile
      (setq projectile-completion-system 'ivy))

    ;; Integration with `magit'
    (with-eval-after-load 'magit
      (setq magit-completing-read-function 'ivy-completing-read)))

  (use-package amx
    :straight t
    :init (setq amx-history-length 20))

  ;; Additional key bindings for Ivy
  (use-package ivy-hydra
    :straight t
    :commands ivy-hydra-read-action
    :init (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :straight t
    :init
    (setq counsel-projectile-grep-initial-input '(ivy-thing-at-point))
    (counsel-projectile-mode 1))

  ;; Integrate yasnippet
  (use-package ivy-yasnippet
    :straight t
    :commands ivy-yasnippet--preview
    ; :bind ("C-c C-y" . ivy-yasnippet)
    :config (advice-add #'ivy-yasnippet--preview :override #'ignore))

  ;; Select from xref candidates with Ivy
  (use-package ivy-xref
    :straight t
    :init
    (when (boundp 'xref-show-definitions-function)
      (setq xref-show-definitions-function #'ivy-xref-show-defs))
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :straight t
    :bind (:map counsel-mode-map
           ("C-c c v" . counsel-tramp)))
  )

;; More friendly display transformer for Ivy
(use-package ivy-rich
  :straight t
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1))))

(provide 'init-ivy)

;;; init-ivy.el ends here
