;;; -*- lexical-binding: t -*-

(global-company-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)

(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)
(setq company-backends (delete 'company-clang company-backends))
(with-eval-after-load 'company
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(provide 'init-company)
