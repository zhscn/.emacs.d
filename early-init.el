;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold 500000000)

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(setq visible-cursor nil)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(advice-add #'x-apply-session-resources :override #'ignore)

(provide 'early-init)
;;; early-init.el ends here
