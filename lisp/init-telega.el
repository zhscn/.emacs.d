;;; -*- lexical-binding: t; -*-
(straight-use-package '(telega :type git :host github :branch "master"))

(setq telega-use-images t
      telega-open-file-function 'org-open-file
      telega-filter-button-width '(0.25 8 17)
      telega-filters-custom
      '(("Main" . main)
        ("Groups" . (type basicgroup supergroup))
        ("Channels" . (type channel))
        ("Archive" . archive))
      telega-proxies '((:server "localhost" :port 1080 :enable t :type (:@type "proxyTypeHttp"))))

(defun +telega-open-file (file)
  (cond
   ((member (downcase (file-name-extension file)) '("png" "jpg" "gif" "jpeg"))
    (start-process "telega-open-photo" nil "/bin/imv" file))
   ((member (downcase (file-name-extension file)) '("mp4"))
    (start-process "telega-open-video" nil "/bin/mpv" file))
   (t
    (find-file file))))

(defun +use-fixed-pitch ()
  (interactive)
  (make-face 'mytelega-face)
  (set-face-attribute 'mytelega-face nil :font (font-spec :family "Sarasa Fixed SC"))
  (setq buffer-face-mode-face 'mytelega-face)
  (make-variable-buffer-local 'face-font-rescale-alist)
  (add-to-list 'face-font-rescale-alist '("-Noto Sans-" . 0.6))
  (add-to-list 'face-font-rescale-alist '("-Noto Sans Math-" . 0.5))
  (add-to-list 'face-font-rescale-alist '("-Noto Color Emoji-" . 0.7))
  (add-to-list 'face-font-rescale-alist '("-Noto Sans Georgian-" . 0.8))
  (add-to-list 'face-font-rescale-alist '("-Symbola-" . 0.8))
  (buffer-face-mode +1))

(unless window-system
  (setq telega-open-message-as-file '(photo video)
        telega-open-file-function '+telega-open-file))

(setq telega-chat-input-markups '(nil "org" "markdown2"))
(add-hook 'telega-chat-mode-hook #'+use-fixed-pitch)

(provide 'init-telega)
