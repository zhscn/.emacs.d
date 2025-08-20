;;; -*- lexical-binding: t -*-

(leaf projectile
  :straight t
  :require t
  :config
  (projectile-mode 1)
  (defun proj-relative-buf-name ()
    (ignore-errors
      (rename-buffer
       (file-relative-name buffer-file-name (projectile-project-root)))))
  (add-hook 'find-file-hook #'proj-relative-buf-name)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name)))))

(leaf rg
  :straight t
  :require t)

(leaf ccls
  :straight t
  :require t
  :config
  (defun ccls-inheritance-hierarchy-derived () (interactive) (ccls-inheritance-hierarchy t))
  (defun ccls-inheritance-hierarchy-base () (interactive) (ccls-inheritance-hierarchy nil))
  (defun ccls-member-variable () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 4)))
  (defun ccls-member-function () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 3)))
  (defun ccls-member-type () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 2)))
  (defun ccls-callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))

  (define-key lsp-mode-map (kbd "C-c v c") #'ccls-call-hierarchy)
  (define-key lsp-mode-map (kbd "C-c v e") #'ccls-callee)
  (define-key lsp-mode-map (kbd "C-c v d") #'ccls-inheritance-hierarchy-derived)
  (define-key lsp-mode-map (kbd "C-c v b") #'ccls-inheritance-hierarchy-base)
  (define-key lsp-mode-map (kbd "C-c v s") #'lsp-ivy-workspace-symbol)
  (define-key lsp-mode-map (kbd "C-c v p") #'lsp-ui-peek-find-definitions)
  (define-key lsp-mode-map (kbd "C-c v v") #'ccls-member-variable)
  (define-key lsp-mode-map (kbd "C-c v f") #'ccls-member-function)
  (define-key lsp-mode-map (kbd "C-c v t") #'ccls-member-type)

  (defun load-ccls ()
    (lsp))
  (add-hook 'c-mode-hook #'load-ccls)
  (add-hook 'c++-mode-hook #'load-ccls)
  (setq ccls-executable "~/.local/bin/ccls"))

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-hook 'c++-mode-hook #'tree-sitter-mode)
(add-hook 'c++-mode-hook #'tree-sitter-hl-mode)

(leaf lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-enable-symbol-highlighting nil
        lsp-ui-doc-enable nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-enable nil
        lsp-ui-peek-always-show t
        lsp-lens-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-diagnostics-provider :none
        lsp-eldoc-enable-hover nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        lsp-completion-provider :none
        lsp-completion-show-detail nil
        lsp-completion-show-kind nil)
  (leaf lsp-ivy
    :straight t
    :after lsp)
  (leaf lsp-ui
    :straight t
    :after lsp))

;; (leaf global-tags
;;   :straight (global-tags :type git :host nil :repo "https://git.launchpad.net/global-tags.el")
;;   :require t
;;   :hook ((c-mode-hook c++-mode-hook) . global-tags-shared-backend-mode)
;;   :config
;;   (setq xref-search-program 'ripgrep)
;;   ;; xref (finding definitions, references)
;;   (add-to-list 'xref-backend-functions 'global-tags-xref-backend)
;;   ;; project.el (finding files)
;;   (add-to-list 'project-find-functions 'global-tags-try-project-root)
;;   ;; configure Imenu
;;   (add-hook 'c++-mode-hook #'global-tags-imenu-mode)
;;   ;; to update database after save
;;   (add-hook 'c++-mode-hook (lambda ()
;;                              (add-hook 'after-save-hook
;;                                        #'global-tags-update-database-with-buffer
;;                                        nil
;;                                        t))))

(provide 'init-cpp)
