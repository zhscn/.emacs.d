;;; init-theme.el -*- lexical-binding: t -*-
;;; Commentary: handle theme
;;; Code:

(use-package doom-themes
  :straight t
  :if window-system
  :init
  (add-hook 'after-init-hook (load-theme 'doom-nord-light t))
  (blink-cursor-mode -1)

  (set-face-attribute
    'default nil
    :font (font-spec :family "Consolas"
		     :size 11.5))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
	(frame-parameter nil 'font)
	charset
	(font-spec :family "Sarasa Term SC"
		   :size 11.8)))

  :config
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
    (setq custom-enabled-themes '(doom-one))
    (reapply-themes)))

(if (not window-system)
    (load-theme 'doom-one t))

(use-package doom-modeline
  :straight t
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 10
	doom-modeline-bar-width 3
	doom-modeline-icon nil))

(use-package all-the-icons
  :straight t)

(provide 'init-theme)

;;; init-theme.el ends here
