;;; -*- lexical-binding: t -*-

(leaf ivy
  :straight t
  :hook (after-init-hook . ivy-mode)
  :init
  (setq ivy-height 20
        ivy-wrap t
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

  :config
  (leaf ivy-rich
    :straight t
    :hook (ivy-mode-hook . ivy-rich-mode)
    :config
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

  (leaf ivy-prescient
    :straight t
    :hook (ivy-mode-hook . ivy-prescient-mode))

  (leaf ivy-posframe
    :straight t
    :init (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    :global-minor-mode ivy-posframe-mode)

  (leaf counsel :straight t)
  (leaf swiper :straight t)
  (leaf posframe :straight t)

  :bind
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   ("C-c g" . counsel-git)
   (:minibuffer-local-map
    ("C-r" . counsel-minibuffer-history))))

(provide 'init-complete)
