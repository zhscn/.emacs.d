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
      consult-project-root-function #'consult-project-root)

(defun consult-project-root ()
  "Returns project root directory."
  (when-let (project (project-current))
    (car (project-roots project))))
(keymap-substitute global-map #'switch-to-buffer #'consult-buffer)
(keymap-substitute global-map #'imenu #'consult-imenu)
(keymap-substitute global-map #'isearch-forward #'consult-isearch-history)
(keymap-substitute global-map #'project-find-regexp #'consult-ripgrep)

(puni-global-mode)
(dolist (h '(minibuffer-mode-hook git-rebase-mode-hook))
  (add-hook h #'puni-disable-puni-mode))
(keymap-set puni-mode-map "C-S-<right>" #'puni-slurp-forward)
(keymap-set puni-mode-map "C-S-<left>" #'puni-barf-forward)
(defun my-del (&optional n)
  (interactive "P")
  (if (eq (char-before) 9)
      (backward-delete-char-untabify +1)
    (puni-backward-delete-char n)))
(keymap-set puni-mode-map "<backspace>" #'my-del)
(electric-pair-mode)

(provide 'init-complete)
