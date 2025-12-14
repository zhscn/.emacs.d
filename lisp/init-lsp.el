;; -*- lexical-binding: t; -*-

(setq lsp-enable-symbol-highlighting nil
      lsp-headerline-breadcrumb-enable nil
      lsp-enable-on-type-formatting nil
      lsp-lens-enable nil
      lsp-use-plists t
      lsp-keymap-prefix "C-c l"
      lsp-enable-file-watchers nil
      lsp-java-server-install-dir (expand-file-name "jdtls/" user-emacs-directory)
      lsp-clients-clangd-args '("--header-insertion=never")
      lsp-enable-indentation nil)

(with-eval-after-load "cc-mode"
  (keymap-set c-mode-base-map "RET" #'c-context-line-break)
  (keymap-unset c++-mode-map ":")
  (keymap-unset c++-mode-map ",")
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp))

(with-eval-after-load "lsp-mode"
  (require 'ccls))

(with-eval-after-load "ccls"
  (defun ccls-inheritance-hierarchy-derived () (interactive) (ccls-inheritance-hierarchy t))
  (defun ccls-inheritance-hierarchy-base () (interactive) (ccls-inheritance-hierarchy nil))

  (keymap-set lsp-mode-map "C-c v c" #'ccls-call-hierarchy)
  (keymap-set lsp-mode-map "C-c v d" #'ccls-inheritance-hierarchy-derived)
  (keymap-set lsp-mode-map "C-c v b" #'ccls-inheritance-hierarchy-base)
  (keymap-set lsp-mode-map "C-c v m" #'ccls-member-hierarchy))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'lsp)
(add-hook 'rust-ts-mode-hook #'flycheck-mode)

(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook #'lsp)
(add-hook 'go-mode-hook #'flycheck-mode)

(with-eval-after-load "lsp-mode"
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let* ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

(provide 'init-lsp)
