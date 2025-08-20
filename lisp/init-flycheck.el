;; init-flycheck.el -*- lexical-binding: t -*-
;;; Commentary: Flycheck configurations.
;;; Code:

(use-package flycheck
  :straight t
  :diminish
  :hook (after-init . global-flycheck-mode)
  ; :hook ((prog-mode markdown-mode) . (lambda ()
  ;                                      (unless (string-prefix-p "timemachine:" (buffer-name))
  ;                                        (flycheck-mode 1))))
  :init 
  (flymake-mode -1)
  :config
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode org-mode
              diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe)

  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))

  ;; Display Flycheck errors in GUI tooltips
  (if (display-graphic-p)
    (use-package flycheck-posframe
      :straight t
      :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
      :hook (flycheck-mode . flycheck-posframe-mode)
      :init (setq flycheck-posframe-border-width 1
                  flycheck-posframe-inhibit-functions
                  '((lambda (&rest _) (bound-and-true-p company-backend)))))))

(provide 'init-flycheck)

;;; init-flycheck.el ends here
