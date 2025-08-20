;;; -*- lexical-binding: t -*-
(straight-use-package 'hl-todo)
(straight-use-package 'minions)

(setq column-number-mode t)
(minions-mode)

(setq modus-themes-italic-constructs t
      modus-themes-region '(bg-only no-extend))

(if (display-graphic-p)
    (load-theme 'modus-operandi t)
  (load-theme 'modus-vivendi t))

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(global-hl-todo-mode +1)

(defun +persp-name ()
  (when (length> persp-names-cache 1)
    (format "#%.5s " (safe-persp-name (get-current-persp)))))

(add-to-list 'mode-line-format '((:eval (+persp-name))))

(provide 'init-theme)
