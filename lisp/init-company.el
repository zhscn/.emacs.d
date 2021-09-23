;;; -*- lexical-binding: t -*-

(leaf company
  :straight t
  :require t
  :config
  (global-company-mode 1)
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   (:company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :config
  (add-hook 'company-mode-hook #'(lambda ()
                                   (setq company-backends (delete 'company-clang company-backends))))
  (leaf company-posframe
    :straight t
    :require t
    :config
    (company-posframe-mode 1))
  (leaf company-box
    :straight t
    :hook (company-mode-hook . company-box-mode)))

(leaf smartparens
  :straight t
  :require t
  :config
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(provide 'init-company)
