;;; -*- lexical-binding: t -*-

(setq column-number-mode t)
(minions-mode)

(when window-system
  (global-page-break-lines-mode)
  (set-fontset-font t 'emoji (font-spec :family (if *is-mac* "Apple Color Emoji" "Noto Color Emoji")))
  (set-face-attribute
   'default nil :font (font-spec :family "MonoLisa" :size 16))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "LXGW WenKai Mono"))))

(global-hl-todo-mode +1)

(defun +persp-name ()
  (when (length> persp-names-cache 1)
    (format "#%.5s " (safe-persp-name (get-current-persp)))))

(add-to-list 'mode-line-format '((:eval (+persp-name))))

(provide 'init-theme)
