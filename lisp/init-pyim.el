;;; init-pyim.el -*- lexical-binding: t -*-
;;; Commentary: input method in Emacs
;;; Code:

(use-package liberime
  :straight (liberime
             :type git
             :host github
             :repo "merrickluo/liberime"
             :files ("CMakeLists.txt" "Makefile" "src" "liberime*.el" "liberime-config.el"))
  :init
  (add-hook 'liberime-after-start-hook
            (lambda ()
              (liberime-select-schema "double_pinyin_flypy"))))

(use-package pyim
  :straight t
  :init
  (liberime-load)
  (setq-default pyim-title "pyim")
  :config
  (use-package pyim-basedict
    :straight t
    :config (pyim-basedict-enable))

  (use-package posframe
    :straight t
    :init (require 'posframe))

  (pyim-isearch-mode 1)
  (setq pyim-punctuation-translate-p '(no auto yes)
        default-input-method "pyim"
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
  (("M-k" . pyim-convert-string-at-point)
   ("C-;" . pyim-delete-word-from-personal-buffer)
   ("C-\\". toggle-input-method)
   ("C-'" . pyim-punctuation-translate-at-point)))

(provide 'init-pyim)

;;; init-rust.el ends here
