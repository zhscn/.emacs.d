;;; -*- lexical-binding: t -*-

(leaf doom-themes
  :straight t
  :require t
  :config
  (load-theme 'doom-vibrant t))

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 18))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))
(leaf doom-modeline
  :straight t
  :init
  (setq doom-modeline-height 0)
  (setq doom-modeline-bar-width 0)
  (setq doom-modeline-icon nil)
  :advice (:around meow-setup-indicator doom-modeline-mode))

(provide 'init-theme)
