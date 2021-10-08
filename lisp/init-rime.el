;;; -*- lexical-binding: t; -*-
(straight-use-package '(rime :type git :host github :repo "DogLooksGood/emacs-rime"))

(setq default-input-method "rime"
      rime-title "im")

(when *is-mac*
  (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@28/include"))

(defun +rime-predicate-org-syntax-punc-p ()
  (when (eq major-mode 'org-mode)
    (member rime--current-input-key '(91 93 42 126))))

(defun +rime-predicate-md-syntax-punc-p ()
  (when (eq major-mode 'markdown-mode)
    (member rime--current-input-key '(91 93 96))))
(setq
 rime-disable-predicates '(meow-normal-mode-p
                           meow-motion-mode-p
                           meow-keypad-mode-p
                           +rime-predicate-org-syntax-punc-p
                           +rime-predicate-md-syntax-punc-p
                           rime-predicate-evil-mode-p
                           rime-predicate-after-ascii-char-p
                           rime-predicate-hydra-p
                           rime-predicate-space-after-cc-p
                           rime-predicate-prog-in-code-p)
 rime-inline-predicates '(rime-predicate-space-after-cc-p
                          rime-predicate-current-uppercase-letter-p
                          +rime-predicate-md-syntax-punc-p))
(setq rime-translate-keybindings
      '("C-f" "C-b" "C-n" "C-p" "M-n" "M-p"))
(setq rime-show-candidate 'posframe)
(setq rime-posframe-properties
      (list :font "Sarasa Fixed SC-12"
            :internal-border-width 2))

(with-eval-after-load "rime"
  (define-key rime-mode-map (kbd "M-k") #'rime-force-enable))

(provide 'init-rime)
