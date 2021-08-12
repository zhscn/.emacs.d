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

(defvar +pdump-load-path nil
  "The load-path backup before dump.

This variable is non-nil when emacs is started with dump file.")

(defvar +pdump-packages nil
  "A list of package names to dump.")

(defun +pdump-packages (&rest pkgs)
  "Mark pkgs should be dumped."
  (dolist (pkg pkgs)
    (push pkg +pdump-packages)))

(provide 'init-straight)
