;;; init-theme.el -*- lexical-binding: t -*-
;;; Commentary: handle theme
;;; Code:

;; 11.5 11.8
;; 14.5 14.8
;; 15.5 17.8
;; 12.5 13.5 Source Han Sans HW SC
(use-package doom-themes
  :hook (after-init . (lambda ()
                        (if (not window-system)
                            (progn
                              (load-theme 'doom-vibrant t) ;; doom-one doom-city-light doom-city-lights
                              (xterm-mouse-mode))
                          (progn
                            (load-theme 'doom-nord-light t) ;; doom-acario-light
                            (set-face-attribute
                             'default nil :font (font-spec :family "JetBrains Mono" :size 11.5))
                            (dolist (charset '(kana han symbol cjk-misc bopomofo))
                              (set-fontset-font (frame-parameter nil 'font)
                                                charset (font-spec :family "Sarasa Term SC" :size 13.5)))))))

  :config
  (blink-cursor-mode -1)
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (setq custom-safe-themes t)
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  (defun light ()
    "Activate a light color theme."
    (interactive)
    (setq custom-enabled-themes '(doom-nord-light))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(doom-vibrant))
    (reapply-themes)))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 10
        doom-modeline-bar-width 3
        doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-icon nil))

(provide 'init-theme)

;;; init-theme.el ends here
