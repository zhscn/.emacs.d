;;; -*- lexical-binding: t -*-
(straight-use-package 'company)
(straight-use-package 'posframe)
(straight-use-package 'company-posframe)
(straight-use-package 'company-box)
(straight-use-package 'smartparens)

(global-company-mode +1)

(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(add-hook 'company-mode-hook
          #'(lambda ()
              (setq company-backends (delete 'company-clang company-backends))))

(add-hook 'company-mode-hook #'company-posframe-mode)
(add-hook 'company-mode-hook #'company-box-mode)

(smartparens-global-mode +1)
(show-smartparens-global-mode +1)

(provide 'init-company)
