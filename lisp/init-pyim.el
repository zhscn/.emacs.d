;;; init-pyim.el -*- lexical-binding: t -*-
;;; Commentary: input method in Emacs
;;; Code:

(use-package pyim
  :init
  (add-to-list 'load-path (expand-file-name "~/pkg/liberime"))
  (require 'liberime)
  (liberime-select-schema "double_pinyin_flypy")
  (liberime-load)
  :demand t
  :config
  (use-package pyim-basedict
    :ensure nil
    ; :hook (after-init . (lambda () (pyim-restart-1 t)))
    ; :init (setq pyim-dicts
    ;             '((:name "base" :file "/home/zhscn/.emacs.d/pyim/dcache/pyim-bigdict.pyim")))
    :config
    (pyim-basedict-enable))

  (use-package posframe
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
