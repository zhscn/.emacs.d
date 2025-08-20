;;; init-rust.el -*- lexical-binding: t -*-
;;; Commentary: rust
;;; Code:

(use-package rust-mode
  :init (setq rust-format-on-save t)
  :config
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode)
    :config
    (setq compilation-filter-hook
          (append compilation-filter-hook '(cargo-process--add-errno-buttons)))))

(provide 'init-rust)

;;; init-rust.el ends here
