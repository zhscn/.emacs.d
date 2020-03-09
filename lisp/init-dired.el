;;; init-dired.el -*- lexical-binding: t -*-
;;; Commentary: dired configuration
;;; Code:

(put 'dired-find-alternate-file 'disabled nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(provide 'init-dired)

;;; init-dired.el ends here
