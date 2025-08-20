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
  (defun my-prettify-c-block-comment (orig-fun &rest args)
    (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
           (star-col-num (when first-comment-line
                           (save-excursion
                             (re-search-backward "/\\*")
                             (1+ (current-column))))))
      (apply orig-fun args)
      (when first-comment-line
        (save-excursion
          (newline)
          (dotimes (cnt star-col-num)
            (insert " "))
          (move-to-column star-col-num)
          ; (insert "*/")
          )
        (move-to-column star-col-num) ; comment this line if using bsd style
        (insert "*") ; comment this line if using bsd style
       ))
    ;; Ensure one space between the asterisk and the comment
    (when (not (looking-back " "))
      (insert " ")))
  (advice-add 'c-indent-new-comment-line :around #'my-prettify-c-block-comment)
  (use-package clang-format
    :straight t
    :config
    (setq clang-format-style-option "file"))

  (use-package modern-cpp-font-lock
    :straight t
    :init (modern-c++-font-lock-global-mode t)))

;;(add-hook 'c++-mode #'lsp-deferred)
;;(ccls-use-default-rainbow-sem-highlight)
;;(face-spec-set 'ccls-sem-member-face
;;               '((t :slant "normal"))
;;               'face-defface-spec)

(provide 'init-cc)

;;; init-cc.el ends here
