;;; -*- lexical-binding: t -*-
(straight-use-package 'doom-modeline)

(setq modus-themes-region '(bg-only no-extend))
(load-theme 'modus-vivendi t)

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(setq doom-modeline-height 0
      doom-modeline-bar-width 0
      doom-modeline-icon nil)

(advice-add 'meow-setup-indicator :around #'doom-modeline-mode)

(provide 'init-theme)
