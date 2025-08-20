;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)
(straight-use-package 'org-roam)

(straight-use-package 'ebib)
(straight-use-package 'citar)
(straight-use-package 'citeproc)
(straight-use-package 'xenops)

(setq org-directory (file-truename "~/org/"))

;;; citar
(setq bibtex-completion-bibliography `(,(concat org-directory "bibliography/ref.bib"))
      bibtex-completion-library-path `(,(concat org-directory "pdf/"))
      bibtex-completion-notes-path (concat org-directory "roam/"))

(setq citar-bibliography bibtex-completion-bibliography
      citar-notes-paths `(,(concat org-directory "roam/"))
      citar-library-paths bibtex-completion-library-path
      citar-file-note-org-include '(org-id org-roam-ref)
      citar-templates '((main . "${author editor:30}     ${date year issued:4}     ${title:48}")
                        (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
                        (note . "#+title: ${title}")))

(with-eval-after-load "embark"
  (setq citar-at-point-function 'embark-act))
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;; org-cite
(with-eval-after-load "citar"
  (require 'ox)
  (require 'oc)
  (require 'citar-org)
  (require 'org-roam))

(setq org-cite-global-bibliography citar-bibliography
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;;; org-roam
(setq org-roam-v2-ack t
      org-roam-directory (concat org-directory "roam"))

(with-eval-after-load "org-roam"
  (org-roam-db-autosync-mode +1))

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

(setq org-agenda-text-search-extra-files (directory-files-recursively org-roam-directory "\\.org$")
      org-html-validation-link nil)

;; (leaf org-latex-impatient
;;   :straight t
;;   :require t
;;   :hook (org-mode-hook . org-latex-packages-mode)
;;   :init
;;   (setq org-latex-impatient-tex2svg-bin "~/.local/package/node_modules/mathjax-node-cli/bin/tex2svg"))

(provide 'init-org)
