;;; init-lsp.el -*- lexical-binding: t -*-
;;; Commentary: working like ide
;;; Code:

(use-package lsp-mode
  :straight t
  :commands lsp
  :bind
  (:map lsp-mode-map
    ("C-c C-d" . lsp-describe-thing-at-point))
  :config
  (setq lsp-prefer-flymake nil))

; (use-package lsp-ui
;   :straight t
;   :custom-face
;   (lsp-ui-sideline-code-action ((t (:inherit warning))))
;   :bind (("C-c u" . lsp-ui-imenu)
;          :map lsp-ui-mode-map
;          ("M-<f6>" . lsp-ui-hydra/body))
;   :hook (lsp-mode . lsp-ui-mode)
;   :init (setq lsp-ui-doc-enable t
;               lsp-ui-doc-use-webkit nil
;               lsp-ui-doc-delay 0.2
;               lsp-ui-doc-include-signature t
;               lsp-ui-doc-position 'at-point
;               lsp-ui-doc-border (face-foreground 'default)
;               lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

;               lsp-ui-sideline-enable t
;               lsp-ui-sideline-show-hover nil
;               lsp-ui-sideline-show-diagnostics nil
;               lsp-ui-sideline-ignore-duplicate t

;               lsp-ui-imenu-enable t
;               lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
;                                     ,(face-foreground 'font-lock-string-face)
;                                     ,(face-foreground 'font-lock-constant-face)
;                                     ,(face-foreground 'font-lock-variable-name-face)))
;   :config
;   (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

;   ;; `C-g'to close doc
;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

;   ;; Reset `lsp-ui-doc-background' after loading theme
;   (add-hook 'after-load-theme-hook
;             (lambda ()
;               (setq lsp-ui-doc-border (face-foreground 'default))
;               (set-face-background 'lsp-ui-doc-background
;                                    (face-background 'tooltip)))))

(use-package company-lsp
  :straight t
  :commands company-lsp
  :init
  (setq-default lsp-auto-guess-root nil
    company-transformers nil
    company-lsp-async t
    company-lsp-cache-candidates t))

(use-package yasnippet
    :straight t
    ; :diminish yas-minor-mode
    :hook (after-init . yas-global-mode)
    :config
    (yas-minor-mode)
    (yas-reload-all)
    (use-package yasnippet-snippets
      :straight t
      :after yasnippet))

; (use-package dap-mode
;   :straight t
;   :diminish
;   :bind
;   (:map dap-mode-map
;         (("<f12>" . dap-debug)
;          ("<f8>" . dap-continue)
;          ("<f9>" . dap-next)
;          ("<M-f11>" . dap-step-in)
;          ("C-M-<f11>" . dap-step-out)
;          ("<f7>" . dap-breakpoint-toggle)))
;   :config
;   (require 'dap-gdb-lldb)
;   (dap-mode 1)
;   (dap-ui-mode 1)
;   ;; enables mouse hover support
;   (dap-tooltip-mode 1)
;   ;; use tooltips for mouse hover
;   ;; if it is not enabled `dap-mode' will use the minibuffer.
;   (tooltip-mode 1))

(provide 'init-lsp)

;;; init-lsp.el ends here
