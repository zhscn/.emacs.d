;;; -*- lexical-binding: t -*-

;; vertico
(add-hook 'after-init-hook #'vertico-mode)
(setq vertico-cycle t)
(savehist-mode +1)

;; vertico-directory
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(with-eval-after-load "vertico"
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word))

;; marginalia
(add-hook 'vertico-mode-hook #'marginalia-mode)

;; orderless
(setq completion-styles '(orderless)
      completion-category-defaults nil)

(advice-add 'company-capf
            :around
            (lambda (capf-fn &rest args)
              (let ((completion-styles '(basic partial-completion substring)))
                (apply capf-fn args))))

;; consult
(setq consult-narrow-key "<"
      consult-project-root-function #'consult-project-root
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(defun consult-project-root ()
  "Returns project root directory."
  (when-let (project (project-current))
    (car (project-roots project))))
(keymap-substitute global-map #'switch-to-buffer #'consult-buffer)
(keymap-substitute global-map #'goto-line #'consult-goto-line)
(keymap-substitute global-map #'imenu #'consult-imenu)
(keymap-substitute global-map #'isearch-forward #'consult-isearch-history)
(keymap-substitute global-map #'project-find-regexp #'consult-ripgrep)
(with-eval-after-load "org-mode"
  (keymap-substitute org-mode-map #'consult-imenu #'consult-org-heading))

(puni-global-mode)
(add-hook 'minibuffer-mode-hook #'puni-disable-puni-mode)
(add-hook 'git-rebase-mode-hook #'puni-disable-puni-mode)
(electric-pair-mode)

(provide 'init-complete)
