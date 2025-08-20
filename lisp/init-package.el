;;; -*- lexical-binding: t -*-

(setq comp-deferred-compilation-deny-list ())
(setq straight-vc-git-default-clone-depth 1)

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
	    (not (native-comp-available-p))))

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '((nil . "default.el")
        ;; Packages which are pinned to a specific commit.
        (pinned . "pinned.el")))

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

(autoload #'straight-x-pull-all "straight-x" nil t)
(autoload #'straight-x-freeze-versions "straight-x" nil t)

(straight-use-package 'gcmh)
(gcmh-mode +1)

(straight-use-package 'esup)
(autoload 'esup "esup" nil t)

(straight-use-package 'exec-path-from-shell)
(setq exec-path-from-shell-variables '("PATH" "MANPATH"))
(setq exec-path-from-shell-check-startup-files nil)
(setq exec-path-from-shell-arguments '("-l" ))

(when *is-mac*
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(provide 'init-package)
