;;; -*- lexical-binding: t -*-

(setq comp-deferred-compilation-deny-list ())
(setq straight-vc-git-default-clone-depth 1)

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
	(not (native-comp-available-p))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; <leaf-install-code>
(eval-when-compile
  (straight-use-package 'leaf)
  (straight-use-package 'leaf-keywords)
  (require 'leaf)
  (require 'leaf-keywords)
  (leaf-keywords-init))
;; </leaf-install-code>

(leaf gcmh
  :straight t
  :require t
  :config
  (gcmh-mode 1))

(provide 'init-package)
