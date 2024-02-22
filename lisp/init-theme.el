;;; -*- lexical-binding: t -*-

(setq column-number-mode t)
(minions-mode)

(setq modus-themes-italic-constructs t
      modus-themes-region '(bg-only no-extend))

(load-theme 'modus-vivendi t)

(when window-system
  (global-page-break-lines-mode)
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
  ;; (set-fontset-font t 'unicode "Apple Color Emoji" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Cascadia Mono" :size 16))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "LXGW WenKai Mono"))))

(global-hl-todo-mode +1)

(defun +persp-name ()
  (when (length> persp-names-cache 1)
    (format "#%.5s " (safe-persp-name (get-current-persp)))))

(add-to-list 'mode-line-format '((:eval (+persp-name))))

(provide 'init-theme)
