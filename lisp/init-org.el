;;; -*- lexical-binding: t; -*-

(leaf org
  :straight (org :type git :host github :repo "bzg/org-mode")
  :require t)

(leaf org-roam
  :straight (org-roam :type git :host github :repo "org-roam/org-roam")
  :require t
  :init
  ;; (setq org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))
  (setq org-roam-v2-ack t)
  (setq org-roam-directory (file-truename "~/org")))

(leaf org-tree-slide
  :straight t
  :require t
  :config
  (leaf darkroom
    :straight t
    :hook org-tree-slide-mode-hook darkroom-mode)

  (defvar-local org-tree-slide--on nil)
  (defun org-tree-slide-toggle ()
    (interactive)
    (if (not org-tree-slide--on)
        (progn
          (setq-local org-tree-slide--on t)
          (org-tree-slide-mode +1)
          (darkroom-mode +1))
      (progn
        (setq-local org-tree-slide--on nil)
        (org-tree-slide-mode -1)
        (darkroom-mode -1))))
  :bind ((:org-mode-map
          ("<f8>" . org-tree-slide-toggle))
         (:org-tree-slide-mode-map
          ("<f9>" . org-tree-slide-move-previous-tree)
          ("<f10>" . org-tree-slide-move-next-tree))))

;; (leaf org-latex-impatient
;;   :straight t
;;   :require t
;;   :hook (org-mode-hook . org-latex-packages-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin "~/.local/package/node_modules/mathjax-node-cli/bin/tex2svg"))

(provide 'init-org)
