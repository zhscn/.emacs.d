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

(setq ad-redefinition-action 'accept
      apropos-do-all t
      auto-mode-case-fold nil
      auto-save-default nil
      display-line-numbers-width-start t
      make-backup-files nil
      ring-bell-function 'ignore
      idle-update-delay 1)

(setq-default truncate-lines t)

(set-buffer-file-coding-system 'utf-8-unix)
(set-clipboard-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "*" 'utf-8-unix)
; (set-language-environment 'utf-8)  ;; enable these two options will deactive font configuration on windows
; (setq locale-coding-system 'utf-8  ;; refer to https://emacs-china.org/t/topic/4581
;       default-process-coding-system '(utf-8 . utf-8))

(when (eq system-type 'windows-nt)
  (set-default-coding-systems 'utf-8-unix)
  (set-selection-coding-system 'utf-8-unix)
  ; (setq abbreviated-home-dir "\\`'")
  (setq default-directory "d:/"))

(setq inhibit-compacting-font-caches t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen -1
      inhibit-default-init t
      ; initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

; (setq initial-frame-alist
;       '((width . 100) (height . 35)))

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
