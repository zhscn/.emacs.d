;;; -*- lexical-binding: t -*-

(leaf projectile
  :straight t
  :global-minor-mode projectile-mode
  :config
  ;; (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (defun proj-relative-buf-name ()
    (ignore-errors
      (rename-buffer
       (file-relative-name buffer-file-name (projectile-project-root)))))
  (add-hook 'find-file-hook #'proj-relative-buf-name))

(leaf persp-mode
  :straight t
  :require t
  :config
  (set-persp-parameter 'dont-save-to-file t nil)
  (persp-mode 1))

(leaf rg
  :straight t
  :require t)

(require 'cmake-mode)

(leaf ccls
  :straight t
  :init
  (setq ccls-executable "~/.local/bin/ccls")
  (defvar ccls-loaded nil)
  (defun load-ccls ()
    (unless ccls-loaded
      (require 'ccls)
      (setq ccls-loaded t))
    (lsp))
  (add-hook 'c-mode-hook #'load-ccls)
  (add-hook 'c++-mode-hook #'load-ccls)

  :config
  (with-eval-after-load "ccls"
    (defun ccls-inheritance-hierarchy-derived () (interactive) (ccls-inheritance-hierarchy t))
    (defun ccls-inheritance-hierarchy-base () (interactive) (ccls-inheritance-hierarchy nil))
    (defun ccls-member-variable () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 4)))
    (defun ccls-member-function () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 3)))
    (defun ccls-member-type () (interactive) (lsp-ui-peek-find-custom "$ccls/member" '(:kind 2)))
    (defun ccls-callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
    (defun ccls-navigate-j () (interactive) (ccls-navigate "D"))
    (defun ccls-navigate-k () (interactive) (ccls-navigate "U"))
    (defun ccls-navigate-h () (interactive) (ccls-navigate "L"))
    (defun ccls-navigate-l () (interactive) (ccls-navigate "R"))

    (define-key lsp-mode-map (kbd "C-c v c") #'ccls-call-hierarchy)
    (define-key lsp-mode-map (kbd "C-c v e") #'ccls-callee)
    (define-key lsp-mode-map (kbd "C-c v d") #'ccls-inheritance-hierarchy-derived)
    (define-key lsp-mode-map (kbd "C-c v b") #'ccls-inheritance-hierarchy-base)
    (define-key lsp-mode-map (kbd "C-c v s") #'lsp-ivy-workspace-symbol)
    (define-key lsp-mode-map (kbd "C-c v p") #'lsp-ui-peek-find-definitions)
    (define-key lsp-mode-map (kbd "C-c v v") #'ccls-member-variable)
    (define-key lsp-mode-map (kbd "C-c v f") #'ccls-member-function)
    (define-key lsp-mode-map (kbd "C-c v t") #'ccls-member-type)
    (define-key lsp-mode-map (kbd "C-c v h") #'ccls-navigate-h)
    (define-key lsp-mode-map (kbd "C-c v j") #'ccls-navigate-j)
    (define-key lsp-mode-map (kbd "C-c v k") #'ccls-navigate-k)
    (define-key lsp-mode-map (kbd "C-c v l") #'ccls-navigate-l)))

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

  (defun lsp-hover-manually ()
    (interactive)
    (lsp-request-async
         "textDocument/hover"
         (lsp--text-document-position-params)
         (-lambda ((hover &as &Hover? :range? :contents))
           (when hover
             (when range?
               (setq lsp--hover-saved-bounds (lsp--range-to-region range?)))
             (lsp--eldoc-message (and contents
                                      (lsp--render-on-hover-content
                                       contents
                                       lsp-eldoc-render-all)))))
         :error-handler #'ignore
         :mode 'tick
         :cancel-token :eldoc-hover))
  (define-key lsp-mode-map (kbd "C-;") #'lsp-hover-manually)

  (leaf lsp-ivy
    :straight t
    :after lsp)
  (leaf lsp-ui
    :straight t
    :after lsp))

(provide 'init-cpp)
