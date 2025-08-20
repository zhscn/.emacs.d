;;; -*- lexical-binding: t -*-

(global-company-mode +1)
(company-prescient-mode +1)
(prescient-persist-mode +1)
(global-copilot-mode)

(keymap-set company-active-map "C-n" #'company-select-next)
(keymap-set company-active-map "C-p" #'company-select-previous)

(with-eval-after-load 'company
  (delq 'company-clang company-backends)
  ;; disable inline previews
  (delq 'company-preview-if-just-one-frontend company-frontends))

(with-eval-after-load "copilot"
  (keymap-set copilot-completion-map "<tab>" 'copilot-accept-completion)
  (keymap-set copilot-completion-map "TAB" 'copilot-accept-completion))

(provide 'init-company)
