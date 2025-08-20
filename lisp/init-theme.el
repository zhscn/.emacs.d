;;; -*- lexical-binding: t -*-
(straight-use-package 'hl-todo)

(setq modus-themes-region '(bg-only no-extend))
(load-theme 'modus-vivendi t)

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(global-hl-todo-mode +1)

(provide 'init-theme)
