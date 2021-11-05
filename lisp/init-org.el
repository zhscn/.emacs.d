;;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type git :host github :repo "bzg/org-mode"))
(straight-use-package 'org-tree-slide)
(straight-use-package 'darkroom)
(straight-use-package 'org-roam)

(straight-use-package 'ebib)
(straight-use-package 'bibtex-actions)
(straight-use-package 'citeproc)
(straight-use-package 'xenops)

(setq org-directory (file-truename "~/org/"))

;;; bibtex-actions
(setq bibtex-completion-bibliography `(,(concat org-directory "bibliography/ref.bib"))
      bibtex-completion-library-path `(,(concat org-directory "pdf/"))
      bibtex-completion-notes-path (concat org-directory "roam")
      bibtex-actions-bibliography bibtex-completion-bibliography
      bibtex-actions-file-note-org-include '(org-id org-roam-ref)
      bibtex-actions-file-open-note-function #'+org-insert-id-refs-custom)

(with-eval-after-load "embark"
  (setq bibtex-actions-at-point-function 'embark-act)
  (add-to-list 'embark-keymap-alist '(bib-reference . bibtex-actions-map))
  (add-to-list 'embark-keymap-alist '(citation-key . bibtex-actions-buffer-map))
  (with-eval-after-load "bibtex-actions"
    (add-to-list 'embark-target-finders #'bibtex-actions-citation-key-at-point)))
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;; org-cite
(with-eval-after-load "oc"
  (require 'ox)
  (require 'oc-bibtex-actions))
(setq org-cite-global-bibliography bibtex-actions-bibliography
      org-cite-insert-processor 'oc-bibtex-actions
      org-cite-follow-processor 'oc-bibtex-actions
      org-cite-activate-processor 'oc-bibtex-actions)

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

(defun +org-insert-id-refs-custom (key entry)
  "Open a note file from KEY and ENTRY."
  (if-let* ((file
             (caar (bibtex-actions-file--get-note-filename
                    key
                    bibtex-actions-notes-paths '("org"))))
            (file-exists (file-exists-p file)))
      (funcall bibtex-actions-file-open-function file)
    (let* ((uuid (org-id-new))
           (template (bibtex-actions-get-template 'note))
           (note-meta
            (when template
              (bibtex-actions--format-entry-no-widths
               entry
               template)))
           (org-id (when (member 'org-id bibtex-actions-file-note-org-include)
                     (concat "\n:ID:   " uuid)))
           (org-roam-key (when (member 'org-roam-ref bibtex-actions-file-note-org-include)
                           (concat "\n:ROAM_REFS: @" key)))
           (prop-drawer (or org-id org-roam-key))
           (content
            (concat (when prop-drawer ":PROPERTIES:")
                    org-roam-key org-id
                    (when prop-drawer "\n:END:\n")
                    note-meta "\n")))
      (funcall bibtex-actions-file-open-function file)
      ;; This just overrides other template insertion.
      (erase-buffer)
      (when template (insert content)))))

(provide 'init-org)
