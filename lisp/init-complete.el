;;; -*- lexical-binding: t -*-
(straight-use-package '(vertico :files (:defaults "extensions/*")))
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)

;; vertico
(add-hook 'after-init-hook #'vertico-mode)
(setq vertico-cycle t)
(savehist-mode +1)

;; vertico-directory
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
(with-eval-after-load "vertico"
  (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word))

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
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

(setq consult-narrow-key "<"
      consult-project-root-function #'consult-project-root
      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

(defun consult-project-root ()
  "Returns project root directory."
  (when-let (project (project-current))
    (car (project-roots project))))

(define-key global-map [remap switch-to-buffer] #'consult-buffer)
(define-key global-map [remap goto-line] #'consult-goto-line)
(define-key global-map [remap imenu] #'consult-imenu)
(define-key global-map (kbd "C-s") #'consult-isearch-history)
(define-key global-map [remap project-find-regexp] #'consult-ripgrep)
(with-eval-after-load "org-mode"
  (define-key org-mode-map [remap consult-imenu] #'consult-org-heading))

;; embark
(define-key minibuffer-mode-map (kbd "C-.") #'embark-act)
(define-key minibuffer-mode-map (kbd "C-'") #'embark-dwim)
(define-key minibuffer-mode-map (kbd "M-h") #'embark-bindings)

(setq prefix-help-command #'embark-prefix-help-command)

(add-to-list 'display-buffer-alist
             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
               nil
               (window-parameters (mode-line-format . none))))

;; embark-consult
(with-eval-after-load "embark"
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(provide 'init-complete)
