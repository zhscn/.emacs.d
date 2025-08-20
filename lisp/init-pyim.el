;;; init-pyim.el -*- lexical-binding: t -*-
;;; Commentary: input method in Emacs
;;; Code:

(use-package pyim
  :init
  (unless (eq system-type 'windows-nt)
    (add-to-list 'load-path (expand-file-name "~/pkg/liberime")))
  (require 'liberime)
  (liberime-select-schema "double_pinyin_flypy")
  (liberime-load)
  :demand t
  :config
  (use-package pyim-basedict
    :config
    (pyim-basedict-enable))

  (if (window-system)
      (progn
        (use-package posframe
        :init (require 'posframe))
        (setq pyim-page-tooltip 'posframe))
    (setq pyim-page-tooltip 'popup))
  
  (setq default-input-method "pyim")
  (setq pyim-punctuation-translate-p '(no auto yes)
        pyim-default-scheme 'rime
        ;; pyim-punctuation-dict nil
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
