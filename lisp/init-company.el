;;; -*- lexical-binding: t -*-

(setq company-minimum-prefix-length 1)

(global-company-mode +1)

(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)

(with-eval-after-load 'company
  (delq 'company-clang company-backends))

(provide 'init-company)
