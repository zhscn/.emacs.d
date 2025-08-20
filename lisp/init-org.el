;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)
(straight-use-package 'org-roam)

(straight-use-package 'ebib)
(straight-use-package 'bibtex-actions)
(straight-use-package 'citeproc)

(setq org-directory (file-truename "~/org/"))

;;; bibtex-actions
(setq bibtex-actions-bibliography (concat org-directory "bibliography/ref.bib"))
(with-eval-after-load "bibtex-actions"
  (add-to-list 'embark-target-finders 'bibtex-actions-citation-key-at-point)
  (add-to-list 'embark-keymap-alist '(bib-reference . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map)))
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
