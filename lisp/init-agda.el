;;; -*- lexical-binding: t; -*-

(with-eval-after-load "agda2-mode"
  (defun agda-face-mode-variable ()
    (interactive)
    (make-face 'unicode-face)
    (set-face-attribute 'unicode-face nil :font "JetBrains Mono")
    (setq buffer-face-mode-face 'unicode-face)
    (buffer-face-mode))
  (add-hook 'agda2-mode-hook #'agda-face-mode-variable))

(provide 'init-agda)
