;;; init-base.el -*- lexical-binding: t -*-
;;; Commentary: Emacs built-in configuration and proxy
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode)
(global-hl-line-mode)
(global-display-line-numbers-mode)
(column-number-mode)
;; (toggle-truncate-lines)
(global-so-long-mode)
(setq-default tab-width 4)

(setq ad-redefinition-action 'accept
      apropos-do-all t
      auto-mode-case-fold nil
      auto-save-default nil
      display-line-numbers-width-start t
      make-backup-files nil
      ring-bell-function 'ignore
      idle-update-delay 1)

(setq-default truncate-lines t)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "*" 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(when (eq system-type 'windows-nt)
  ; (setq abbreviated-home-dir "\\`'")
  (setq locale-coding-system 'gb18030)
  (setq w32-unicode-filenames 'nil)
  (setq file-name-coding-system 'gb18030)
  (setq default-directory "d:/"))

(unless (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(setq inhibit-compacting-font-caches t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen -1
      inhibit-default-init t
      inhibit-compacting-font-caches t
      ; initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

; (setq initial-frame-alist
;       '((width . 100) (height . 35)))

; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;   "Prevent annoying \"Active processes exist\" query when you quit Emacs."
;   (cl-letf (((symbol-function #'process-list) (lambda ())))
;     ad-do-it))

(setq kill-buffer-query-functions nil)
; (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Network Proxy
(defvar my-proxy "127.0.0.1:1080")
(defvar socks-noproxy)
(defvar socks-server)

(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" my-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,my-proxy)
           ("https" . ,my-proxy)
           ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if socks-noproxy
      (message "Current SOCKS%d proxy is %s:%d"
         (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
  socks-noproxy '("localhost")
  socks-server '("Default server" "127.0.0.1" 1080 5))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
  socks-noproxy nil)
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(setq-default initial-scratch-message
        (concat ";; Happy hacking :)\n\n"))

(provide 'init-base)

;;; init-base.el ends here
