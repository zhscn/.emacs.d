;;; -*- lexical-binding: t; -*-

(leaf proof-general
  :straight t
  :require t
  :init
  (setq pg-init--script-full-path "/home/zhscn/.emacs.d/straight/repos/PG/proof-general.el"
        pg-init--pg-root (file-name-directory pg-init--script-full-path))
  :config
  (setq proof-splash-enable nil
        proof-three-window-enable t
        coq-project-filename "_CoqProject"
        proof-three-window-mode-policy 'hybrid)
  (add-to-list 'meow-mode-state-list '(coq-mode . normal))
  )

(leaf company-coq
  :straight t
  :require t
  :init
  (setq company-coq-features/prettify-symbols-in-terminals nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.v$" . coq-mode))
  (add-hook 'coq-mode-hook 'company-coq-mode))

(provide 'init-coq)
