;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'ivy-bibtex)
(straight-use-package 'org-ref)
(straight-use-package '(org-roam :type git :host github :repo "org-roam/org-roam"))
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)
(straight-use-package 'org-roam-bibtex)

;;; ivy-bibtex
(autoload 'ivy-bibtex "ivy-bibtex" nil t)

(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(setq bibtex-completion-bibliography '("~/Documents/test.bib"))
(setq bibtex-completion-pdf-field "File")
(setq bibtex-completion-notes-path "~/Documents/notes")

;;; org-ref
(setq reftex-default-bibliography '("~/Documents/test.bib"))
(setq org-ref-pdf-directory "File")
(setq org-ref-bibliography-notes "~/Documents/notes")
(require 'org-ref)

;;; org-roam
(autoload 'org-roam-node-find "org-roam" nil t)
(setq org-roam-v2-ack t)
(setq org-roam-directory (file-truename "~/org"))
(setq org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$"))

;;; org-roam-bibtex
(with-eval-after-load "org-roam"
  (require 'org-roam-bibtex))

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

(define-key org-mode-map (kbd "<f8>") 'org-tree-slide-toggle)
(with-eval-after-load "org-tree-slide"
  (define-key org-tree-slide-mode-map (kbd "<f9>") 'org-tree-slide-move-previous-tree)
  (define-key org-tree-slide-mode-map (kbd "<f10>") 'org-tree-slide-move-next-tree))

;; (leaf org-latex-impatient
;;   :straight t
;;   :require t
;;   :hook (org-mode-hook . org-latex-packages-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin "~/.local/package/node_modules/mathjax-node-cli/bin/tex2svg"))

(provide 'init-org)
