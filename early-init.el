;; -*- lexical-binding: t; -*-
(setq byte-compile-warnings '(cl-functions)
      package-enable-at-startup nil)
(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
