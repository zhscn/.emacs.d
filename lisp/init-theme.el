;;; -*- lexical-binding: t -*-

(leaf theme
  :require bespoke-themes solo-jazz-theme paperlike-theme storybook-theme printed-theme
  :config
  (setq bespoke-set-theme 'light
        bespoke-set-mode-line nil)

  (when window-system
    (set-fontset-font t 'unicode "Symbola" nil 'append)
    (set-face-attribute
     'default nil :font (font-spec :family "Sarasa Fixed SC" :size 18))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset (font-spec :family "Sarasa Fixed SC" :size 18)))
    (load-theme 'bespoke t))

  (unless window-system
    (setq bespoke-set-theme 'dark)
    (load-theme 'bespoke t))

  (defun y/auto-update-theme ()
    "depending on time use different theme"
    (let* ((hour (nth 2 (decode-time (current-time))))
           (theme (cond ((<= 7 hour 18) 'solo-jazz)
                        (t              'joker))))
      (when (not (equal (car-safe custom-enabled-themes) theme))
        (setq custom-enabled-themes `(,theme))
        (reapply-themes))
      ;; run that function again next hour
      (run-at-time (format "%02d:%02d" (+ hour 1) 0) nil 'y/auto-update-theme)))

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
    (setq custom-enabled-themes '(solo-jazz))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(joker))
    (reapply-themes)))

(provide 'init-theme)
