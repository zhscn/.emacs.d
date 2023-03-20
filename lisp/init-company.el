;;; -*- lexical-binding: t -*-

(global-company-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)

(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)
(setq company-backends (delete 'company-clang company-backends))

(provide 'init-company)
