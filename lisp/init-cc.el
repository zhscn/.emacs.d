;;; init-cc.el -*- lexical-binding: t -*-
;;; Commentary: use ccls and clang-format
;;; Code:

(use-package ccls
  :straight t
  :hook ((c-mode c++-mode) .
	 (lambda ()
	   (if (eq system-type 'windows-nt)
	     (setq ccls-executable "D:/lib/LLVM/bin/ccls.exe")
	     (setq ccls-executable "~/.local/bin/ccls"))
	   (require 'ccls)
	   (lsp)))
  :init
  (setq-default c-basic-offset 2
		default-tab-width 2
		;;ccls-sem-highlight-method 'overlay
		c-default-style "cc-mode")
		;; flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :config
  (use-package clang-format
    :straight t
    :config
    (setq clang-format-style-option "file")))

;;(add-hook 'c++-mode #'lsp-deferred)
;;(ccls-use-default-rainbow-sem-highlight)
;;(face-spec-set 'ccls-sem-member-face
;;               '((t :slant "normal"))
;;               'face-defface-spec)

(provide 'init-cc)

;;; init-cc.el ends here
