;;; -*- lexical-binding: t -*-

(setq company-minimum-prefix-length 1)

(global-company-mode +1)

(with-eval-after-load 'company
  (delq 'company-clang company-backends))

(provide 'init-company)
