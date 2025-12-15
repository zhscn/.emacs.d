;; -*- lexical-binding: t; -*-

(let ((default-threshold gc-cons-threshold)
      (default-percentage gc-cons-percentage))
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq gc-cons-threshold default-threshold
                    gc-cons-percentage default-percentage))))

(setq byte-compile-warnings '(cl-functions)
      package-enable-at-startup nil)

(setenv "LSP_USE_PLISTS" "true")

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(provide 'early-init)
