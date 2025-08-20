;;; -*- lexical-binding: t -*-
(straight-use-package 'doom-themes)
(straight-use-package 'doom-modeline)

;;; doom themes
(require 'doom-themes)
(load-theme 'doom-vibrant t)

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(setq doom-modeline-height 0)
(setq doom-modeline-bar-width 0)
(setq doom-modeline-icon nil)

(advice-add 'meow-setup-indicator :around #'doom-modeline-mode)

(provide 'init-theme)
