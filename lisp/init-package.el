;;; -*- lexical-binding: t -*-
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (kill-buffer buffer)
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))

(progn
  (elpaca `(,@elpaca-order))
  (elpaca symbol-overlay)
  (elpaca treesit-auto)
  (elpaca page-break-lines)
  (elpaca gcmh)
  (elpaca (transient :host github :repo "magit/transient"))
  (elpaca exec-path-from-shell)
  (elpaca (telega :type git :host github :branch "master"))
  (elpaca yasnippet)
  (elpaca ws-butler)
  (elpaca magit)
  (elpaca hl-todo)
  (elpaca minions)
  (elpaca persp-mode)
  (elpaca nix-mode)
  (elpaca (vertico :files (:defaults "extensions/*")))
  (elpaca marginalia)
  (elpaca orderless)
  (elpaca consult)
  (elpaca puni)
  (elpaca company)
  (elpaca posframe)
  (elpaca ace-window)
  (elpaca avy)
  (elpaca haskell-mode)
  (elpaca proof-general)
  (elpaca company-coq)
  (elpaca paredit)
  (elpaca (rime :host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
  (elpaca sly)
  (elpaca sly-asdf)
  (elpaca sly-quicklisp)
  (elpaca (sly-stepper :host github :repo "joaotavora/sly-stepper" :files (:defaults "*.el" "*.lisp" "*.asd")))
  (elpaca sly-macrostep)
  (elpaca geiser)
  (elpaca geiser-guile)
  (elpaca clojure-mode)
  (elpaca cider)
  (elpaca fold-this)
  (elpaca rg)
  (elpaca lua-mode)
  (elpaca ccls)
  (elpaca lsp-mode)
  (elpaca meow)
  (elpaca flycheck))

(elpaca-wait)

(require 'transient)

(gcmh-mode +1)

(require 'treesit-auto)
(delete 'c treesit-auto-langs)
(delete 'cpp treesit-auto-langs)
(global-treesit-auto-mode)

(setq exec-path-from-shell-variables '("PATH" "MANPATH" "LANG")
      exec-path-from-shell-check-startup-files nil
      exec-path-from-shell-arguments '("-l"))
(when (or *is-mac* (daemonp))
  (exec-path-from-shell-initialize))

(yas-global-mode)

(setq ws-butler-keep-whitespace-before-point nil)
(ws-butler-global-mode)

(require 'persp-mode)
(set-persp-parameter 'dont-save-to-file t nil)
(setq-default persp-auto-save-opt 0)
(persp-mode +1)

(keymap-set global-map "C-x C-o" #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-alist
      '((?x aw-delete-window "Delete Window")
        (?m aw-swap-window "Swap Windows")
        (?M aw-move-window "Move Window")
        (?c aw-copy-window "Copy Window")
        (?n aw-switch-buffer-in-window "Select Buffer")
        (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
        (?c aw-split-window-fair "Split Fair Window")
        (?v aw-split-window-vert "Split Vert Window")
        (?b aw-split-window-horz "Split Horz Window")
        (?o delete-other-windows "Delete Other Windows")
        (?? aw-show-dispatch-help)))

(with-eval-after-load "magit"
  (defun magit-log-dangling ()
    (interactive)
    (magit-log-setup-buffer
     (-filter
      (lambda (x) (not (or (equal "" x) (s-match "error" x))))
      (s-lines
       (shell-command-to-string
        "git fsck --no-reflogs | awk '/dangling commit/ {print $3}'")))
     '("--no-walk" "--color" "--decorate" "--follow")' nil))

  (transient-append-suffix 'magit-log "s"
    '("d" "dangling" magit-log-dangling)))

(provide 'init-package)
