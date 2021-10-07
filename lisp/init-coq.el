;;; -*- lexical-binding: t; -*-
(straight-use-package 'proof-general)
(straight-use-package 'company-coq)

(with-eval-after-load "meow"
  (add-to-list 'meow-mode-state-list '(coq-mode . normal)))

(autoload #'proof-assert-next-command-interactive "proof-general" nil t)
(setq pg-init--script-full-path "~/.emacs.d/straight/repos/PG/proof-general.el"
      pg-init--pg-root (file-name-directory pg-init--script-full-path)
      proof-splash-enable nil
      proof-three-window-enable t
      coq-project-filename "_CoqProject"
      proof-three-window-mode-policy 'hybrid)


(add-hook 'coq-mode-hook #'company-coq-mode)
(setq company-coq-features/prettify-symbols-in-terminals nil)

(provide 'init-coq)
