;;; -*- lexical-binding: t; -*-

(autoload #'fold-this "fold-this" nil t)

(require 'meow)

(setq meow-cursor-type-insert '(bar . 2)
      meow-expand-hint-remove-delay 1.5
      ;; meow-use-cursor-position-hack t
      meow-keypad-leader-dispatch "C-c")
(meow-global-mode +1)

(defun +delete ()
  (interactive)
  (when (meow--allow-modify-p)
    (cond
     ((equal '(expand . join) (meow--selection-type))
      (delete-indentation nil (region-beginning) (region-end)))
     ((region-active-p)
      (delete-region (region-beginning) (region-end)))
     (t (meow-C-d)))))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-leader-define-key
   '("f" . fold-this)
   '("b" . consult-buffer)
   '("i" . symbol-overlay-put)
   '("I" . symbol-overlay-remove-all)
   '("H" . windmove-left)
   '("J" . windmove-down)
   '("K" . windmove-up)
   '("L" . windmove-right)
   '("U" . meow-undo-in-selection)
   '("o" . ace-window)
   '("[" . kmacro-start-macro-or-insert-counter)
   '("]" . kmacro-end-or-call-macro)

   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))

  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-mark-word)
   '("]" . meow-mark-symbol)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-keypad-start)
   '("d" . +delete)
   '("e" . meow-line)
   '("E" . meow-kmacro-lines)
   '("f" . meow-find)
   '("g" . meow-keypad-start)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-keypad-start)
   '("n" . meow-search)
   '("N" . meow-pop-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . avy-goto-char-2)
   '("Q" . consult-goto-line)
   '("r" . meow-kill)
   '("R" . meow-query-replace-regexp)
   '("s" . meow-change)
   '("S" . meow-swap-grab)
   '("t" . meow-query-replace)
   '("u" . meow-undo)
   '("v" . meow-visit)
   '("V" . meow-kmacro-matches)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-keypad-start)
   '("y" . kill-ring-save)
   '("z" . meow-pop-selection)
   '("Z" . meow-pop-all-selection)
   '("'" . meow-join)
   '("/" . find-file)
   '("`" . repeat)
   '("<escape>" . meow-cancel)))

(meow-setup)
;; (meow-setup-line-number)
(meow-setup-indicator)
(meow-thing-register 'angle
                     '(pair ("<") (">"))
                     '(pair ("<") (">")))

(add-to-list 'meow-char-thing-table
             '(?a . angle))
(provide 'init-meow)
