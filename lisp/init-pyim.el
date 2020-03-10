;;; init-pyim.el -*- lexical-binding: t -*-
;;; Commentary: input method in Emacs
;;; Code:

(use-package pyim
  :ensure nil
  :init
  (require 'pyim)
  (add-to-list 'load-path (expand-file-name "~/pkg/liberime/build"))
  (require 'liberime)
  (liberime-start
   (expand-file-name "/usr/share/rime-data")
   (expand-file-name "~/.emacs.d/pyim/rime"))
  (liberime-select-schema "double_pinyin_flypy")
  :demand t
  :config
  (use-package pyim-basedict
    :straight t
    :ensure nil
    :config (pyim-basedict-enable))

  (use-package posframe
    :straight t
    :init (require 'posframe))

  (setq default-input-method "pyim")
  ;;(pyim-isearch-mode 1)
  (setq pyim-punctuation-translate-p '(no auto yes)
        pyim-default-scheme 'rime
        ;; pyim-punctuation-dict nil
        pyim-page-tooltip 'posframe
        pyim-page-length 5)

  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  ;;pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))
  :bind
  (("C-\\". toggle-input-method)
   ("M-k" . pyim-convert-string-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)
   ("M-'" . pyim-punctuation-translate-at-point)))

(provide 'init-pyim)

;;; init-rust.el ends here
