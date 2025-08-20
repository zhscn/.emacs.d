;;; -*- lexical-binding: t -*-
(straight-use-package '(vertico :files (:defaults "extensions/*")))
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'smartparens)

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

;; smartparens
(smartparens-global-mode +1)
(show-smartparens-global-mode +1)
(sp-with-modes
        '(c++-mode objc-mode c-mode)
        (sp-local-pair "{" nil :post-handlers '(:add ("||\n[i]" "RET"))))

(provide 'init-complete)
