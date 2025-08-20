;;; -*- lexical-binding: t -*-

(setq comp-deferred-compilation-deny-list ()
      straight-vc-git-default-clone-depth 1)

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

(straight-use-package 'gcmh)
(gcmh-mode +1)

(straight-use-package 'esup)

(straight-use-package 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "MANPATH")
      exec-path-from-shell-check-startup-files nil
      exec-path-from-shell-arguments '("-l" ))

(when (or *is-mac* (daemonp))
  (exec-path-from-shell-initialize))

(provide 'init-package)
