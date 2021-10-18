;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)

(let ((straight-current-profile 'pinned))
  (add-to-list 'straight-x-pinned-packages
               '("org-roam" . "f819720c510185af713522c592833ec9f2934251"))
  (straight-use-package 'org-roam))

(setq org-directory (file-truename "~/org/"))


;;; org-roam
(setq org-roam-v2-ack t
      org-roam-directory (concat org-directory "roam"))

;;; org-tree-slide
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

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<f8>") #'org-tree-slide-toggle))

(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") #'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") #'org-tree-slide-move-next-tree))

(setq org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))

;; (leaf org-latex-impatient
;;   :straight t
;;   :require t
;;   :hook (org-mode-hook . org-latex-packages-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin "~/.local/package/node_modules/mathjax-node-cli/bin/tex2svg"))

(provide 'init-org)
