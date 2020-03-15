;;; init-org.el -*- lexical-binding: t -*-
;;; Commentary: Org-mode
;;; Code:

(use-package htmlize
  :straight t)
(setq org-html-htmlize-output-type 'css)
(setq org-html-validation-link nil)
(provide 'init-org)

;;; init-org.el ends here
