;;; -*- lexical-binding: t -*-
(straight-use-package 'ivy)
(straight-use-package 'ivy-rich)
(straight-use-package 'ivy-prescient)
(straight-use-package 'counsel)
(straight-use-package 'swiper)

;;; ivy
(setq ivy-height 20
      ivy-fixed-height-minibuffer t
      ivy-read-action-function #'ivy-hydra-read-action
      ivy-read-action-format-function #'ivy-read-action-format-columns
      ;; don't show recent files in switch-buffer
      ivy-use-virtual-buffers nil
      ;; ...but if that ever changes, show their full path
      ivy-virtual-abbreviate 'full
      ;; don't quit minibuffer on delete-error
      ivy-on-del-error-function #'ignore
      ;; enable ability to select prompt (alternative to `ivy-immediate-done')
      ivy-use-selectable-prompt t
      ivy-initial-inputs-alist nil)

(add-hook 'after-init-hook #'ivy-mode)
(add-hook 'ivy-mode-hook #'ivy-rich-mode)
(add-hook 'ivy-mode-hook #'ivy-prescient-mode)
(add-hook 'ivy-mode-hook #'counsel-mode)

(define-key global-map (kbd "C-s") #'swiper)
(define-key global-map (kbd "C-c C-r") #'ivy-resume)
(define-key global-map (kbd "C-x C-f") #'counsel-find-file)
(define-key global-map (kbd "M-x") #'counsel-M-x)
(define-key global-map (kbd "s-x") #'counsel-M-x)
(define-key global-map (kbd "C-c g") #'counsel-git)
(define-key minibuffer-mode-map (kbd "C-r") #'counsel-minibuffer-history)

(provide 'init-complete)
