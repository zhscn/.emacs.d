;;; -*- lexical-binding: t -*-
(straight-use-package 'company)
(straight-use-package 'posframe)
(straight-use-package 'company-posframe)
(straight-use-package 'company-box)
(straight-use-package 'prescient)
(straight-use-package 'company-prescient)

(global-company-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)

(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)

(add-hook 'company-mode-hook
          (lambda ()
            (setq company-backends (delete 'company-clang company-backends))))

(add-hook 'company-mode-hook #'company-posframe-mode)
(add-hook 'company-mode-hook #'company-box-mode)

(provide 'init-company)
