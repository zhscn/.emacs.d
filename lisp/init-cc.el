;;; -*- lexical-binding: t -*-
(straight-use-package 'rg)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(autoload #'cmake-mode "cmake-mode" nil t)

(provide 'init-cc)
