;;; -*- lexical-binding: t; -*-

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(coq-mode . normal)))

(leaf proof-general
  :straight t
  :init
  (setq pg-init--script-full-path "~/.emacs.d/straight/repos/PG/proof-general.el"
        pg-init--pg-root (file-name-directory pg-init--script-full-path))
  :commands proof-assert-next-command-interactive
  :config
  (setq proof-splash-enable nil
        proof-three-window-enable t
        coq-project-filename "_CoqProject"
        proof-three-window-mode-policy 'hybrid))

(leaf company-coq
  :straight t
  :mode ("\\.v$" . coq-mode)
  :hook (coq-mode-hook . company-coq-mode)
  :init
  (setq company-coq-features/prettify-symbols-in-terminals nil))

(provide 'init-coq)
