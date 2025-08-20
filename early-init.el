(setq byte-compile-warnings '(cl-functions)
      package-enable-at-startup nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "theme" user-emacs-directory))

(require 'init-site-lisp)
(require 'init-builtin)
(require 'init-package)

(provide 'early-init)
