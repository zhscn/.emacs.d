;;; -*- lexical-binding: t -*-

(setq-default
 ;; bidi-display-reordering nil
 ;; no start messages
 inhibit-startup-message t
 ;; don't read x resource file
 inhibit-x-resources t
 ;; no welcome screen
 inhibit-splash-screen t
 inhibit-startup-screen t
 ;; no startup messages
 inhibit-startup-echo-area-message t
 frame-inhibit-implied-resize t
 initial-scratch-message ";;; Happy hacking!\n"
 make-backup-files nil
 ;; don't create lockfiles
 create-lockfiles nil
 ;; UTF-8
 buffer-file-coding-system 'utf-8-unix
 default-file-name-coding-system 'utf-8-unix
 default-keyboard-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix)
 default-sendmail-coding-system 'utf-8-unix
 default-terminal-coding-system 'utf-8-unix
 ;; add final newline
 require-final-newline t
 echo-keystrokes 0.01
 show-paren-style 'parenthese
 visible-cursor nil
 ;; indent with whitespace by default
 indent-tabs-mode nil
 ;; Custom file path
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; prefer y or n
 y-or-n-p-use-read-key t
 ;; always follow link
 vc-follow-symlinks t
 ;; disable visual line move
 line-move-visual t
 ;; case insensitive completion
 read-buffer-completion-ignore-case t
 read-file-name-completion-ignore-case t
 ;; use short answer
 read-answer-short t
 ring-bell-function 'ignore
 tramp-default-method "ssh"
 tramp-use-ssh-controlmaster-options nil)

(fset 'yes-or-no-p 'y-or-n-p)

(global-hl-line-mode +1)

(global-auto-revert-mode +1)

;;; No scroll bar
(when (bound-and-true-p scroll-bar-mode)
  (scroll-bar-mode -1))

;;; No tool bar
(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

(defconst *is-mac*
  ;; (memq window-system '(mac ns x))
  (eq system-type 'darwin))

(when *is-mac*
  (setq ns-use-proxy-icon nil))

;;; No menu bar
(unless (and *is-mac* (display-graphic-p))
  (when (bound-and-true-p menu-bar-mode)
    (menu-bar-mode -1)))

;;; No blink cursor
(add-hook 'after-init-hook (lambda () (blink-cursor-mode -1)))

(defun display-startup-echo-area-message ()
  (message ""))

(autoload #'View-scroll-half-page-forward "view")
(autoload #'View-scroll-half-page-backward "view")

(keymap-set global-map "C-v" #'View-scroll-half-page-forward)
(keymap-set global-map "M-v" #'View-scroll-half-page-backward)

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
(autoload #'cmake-mode "cmake-mode" nil t)

(defun my-whitespace ()
  (interactive)
  (setq show-trailing-whitespace t)
  (whitespace-mode))

(add-hook 'prog-mode-hook #'my-whitespace)

(with-eval-after-load "whitespace"
  (setq whitespace-display-mappings '((tab-mark 9 [187 9] [92 9]))
        whitespace-line-column 120))

(defun open-kitty ()
  (interactive)
  (do-applescript "
if application \"Kitty\" is not running then
    launch application \"Kitty\"
else
    tell application \"Kitty\" to activate
end if
"))

(keymap-set global-map "M-s-," #'open-kitty)

(provide 'init-builtin)
