;;; init-company.el -*- lexical-binding: t -*-
;;; Commentary: auto complete
;;; Code:

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next)
         ("<tab>" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-p" . company-select-previous)
         ("C-n" . company-select-next))
  :hook (after-init . global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 0
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 1
        ; company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  ;; Better sorting and filtering
  (use-package company-prescient
    :init (company-prescient-mode 1)))

(provide 'init-company)

;;; init-company.el ends here
