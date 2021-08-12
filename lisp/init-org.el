;;; -*- lexical-binding: t; -*-

(leaf org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam")
  :require t
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/org"))
  ;; (setq org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))
)

;; (leaf org-latex-impatient
;;   :straight t
;;   :require t
;;   :hook (org-mode-hook . org-latex-packages-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin "~/.local/package/node_modules/mathjax-node-cli/bin/tex2svg"))

(provide 'init-org)
