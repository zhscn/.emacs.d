;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'ivy-bibtex)
(straight-use-package 'org-ref)
(straight-use-package 'org-roam-bibtex)
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)

(let ((straight-current-profile 'pinned))
  (add-to-list 'straight-x-pinned-packages
               '("org-roam" . "f819720c510185af713522c592833ec9f2934251"))
  (straight-use-package 'org-roam))

(setq org-directory (file-truename "~/org/"))

;;; ivy-bibtex
(setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))

(setq bibtex-completion-bibliography `(,(concat org-directory "bibliography/ref.bib"))
      bibtex-completion-library-path `(,(concat org-directory "pdf"))
      bibtex-completion-pdf-field "File"
      bibtex-completion-notes-path (concat org-directory "notes"))

;;; org-ref
(setq org-ref-completion-library 'org-ref-ivy-cite
      reftex-default-bibliography bibtex-completion-bibliography
      org-ref-default-bibliography bibtex-completion-bibliography
      org-ref-pdf-directory (file-truename "~/Zotero/storage")
      org-ref-bibliography-notes (concat org-directory "note.org"))

(defun +org-ref-ivy-insert-cite-link (&optional arg)
  "insert cite link with help of `ivy-bibtext' and corresponding
`ivy-bibtex-display-transformer'"
  (interactive "P")
  (setq org-ref-bibtex-files (if arg
				                 org-ref-default-bibliography
			                   (org-ref-find-bibliography)))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (ivy-read "Open: "
              candidates
              :preselect preselect
              :caller 'ivy-bibtex
              :history 'ivy-bibtex-history
              :action 'or-ivy-bibtex-insert-cite)))

;;; org-roam
(setq org-roam-v2-ack t
      org-roam-directory (concat org-directory "roam"))

;;; org-roam-bibtex
(defun load-roam-bibtex ()
  (setq bibtex-ref-roam--loaded t)
    (require 'org-roam)
    (require 'org-ref)
    (require 'ivy-bibtex)
    (require 'org-roam-bibtex)
    (setq org-ref-insert-cite-function #'+org-ref-ivy-insert-cite-link)
    (org-roam-setup)
    (org-roam-bibtex-mode +1))

(with-eval-after-load "org-roam" (load-roam-bibtex))
(with-eval-after-load "ivy-bibtex" (load-roam-bibtex))

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
