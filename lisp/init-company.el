;;; -*- lexical-binding: t -*-

(leaf company
  :straight t
  :global-minor-mode global-company-mode
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   (:company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :config
  (add-hook 'company-mode-hook
            #'(lambda ()
                (setq company-backends (delete 'company-clang company-backends))))

  (leaf company-posframe
    :straight t
    :hook (company-mode-hook . company-posframe-mode))

  (leaf company-box
    :straight t
    :hook (company-mode-hook . company-box-mode)))

(leaf smartparens
  :straight t
  :global-minor-mode smartparens-global-mode show-smartparens-global-mode)

(provide 'init-company)
