;;; -*- lexical-binding: t; -*-
(defconst google-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . t)
    (comment-column . 40)
    (c-indent-comment-alist . ((other . (space . 2))))
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((func-decl-cont . ++)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (inextern-lang . 0)
                        (substatement-open . 0)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          ++))
                        (label . /)
                        (statement-case-open . +)
                        (statement-case-intro . +) ; case w/o {
                        (innamespace . 0))))
  "Modified Google C/C++ Programming Style.")

(defconst fdb-c-style
  `(;; IndentWidth: 4
    (c-basic-offset . 4)
    ;; UseTab: ForIndentation
    (indent-tabs-mode . t)
    ;; TabWidth: 4
    (tab-width . 4)
    ;; ColumnLimit: 120
    (fill-column . 120)
    ;; BreakBeforeBraces: Attach (K&R style, e.g., "if (cond) {")
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-offsets-alist . (
                        ;; AccessModifierOffset: -4
                        (access-label . -4)
                        ;; IndentCaseLabels: false
                        (case-label . 0)
                        ;; NamespaceIndentation: None
                        (innamespace . 0)
                        ;; ConstructorInitializerIndentWidth: 2
                        (member-init-intro . 2)
                        (member-init-cont . 2)
                        ;; IndentWrappedFunctionNames: false
                        (func-decl-cont . 0)
                        ;; AlignAfterOpenBracket: Align
                        (arglist-cont-nonempty . c-lineup-arglist-intro-after-paren)
                        (arglist-cont . c-lineup-arglist-intro-after-paren)
                        (arglist-close . c-lineup-arglist)
                        ;; -- Sensible defaults for C++ --
                        ;; AlwaysBreakTemplateDeclarations: true
                        (topmost-intro . 0)
                        (block-open . 0)
                        (substatement . +)
                        (substatement-open . 0)
                        (statement-case-open . +)
                        (statement-case-intro . +)
                        (inher-intro . +)
                        (inher-cont . c-lineup-multi-inher)
                        (comment-intro . 0)))
    ;; ForEachMacros: [ foreach, Q_FOREACH, BOOST_FOREACH ]
    (c-recognize-foreach-p . t)
    (c-foreach-keywords . '("foreach" "Q_FOREACH" "BOOST_FOREACH")))
  "FoundationDB style based on Mozilla.")

(defvar-local my-c-style "google")

(with-eval-after-load "cc-mode"
  (c-add-style "google" google-c-style nil)
  (c-add-style "fdb" fdb-c-style nil)
  (defun my-c-style-setup ()
    (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (c-set-style my-c-style)))
  (add-hook 'hack-local-variables-hook #'my-c-style-setup))

(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

(with-eval-after-load "cc-mode"
  (defun c-page-break ()
    "insert page break line in comment"
    (interactive)
    (let ((comment (read-from-minibuffer "comment: ")))
      (move-end-of-line nil)
      (insert ?\n)
      (c-indent-line-or-region)
      (let ((eq-str (make-string (/ (- 80 (length comment) 6 (current-column)) 2) ?=)))
        (insert "/// ")
        (insert eq-str)
        (insert " ")
        (insert comment)
        (insert " ")
        (insert eq-str))))
  (keymap-set c-mode-base-map "M-'" #'c-page-break))

(provide 'init-cc)
