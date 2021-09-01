;;; -*- lexical-binding: t; -*-

(leaf meow
  :straight t
  :require t
  :init
  (setq meow-cursor-type-insert '(bar . 2)
        meow-expand-hint-remove-delay 1.5)
  :config
  (meow-global-mode 1)
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("x" . meow-keypad-start)
     '("c" . meow-keypad-start))

    (meow-leader-define-key
     '("x" . meow-motion-origin-command)
     '("c" . meow-motion-origin-command)
     '("b" . switch-to-buffer)

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
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))

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
     '("d" . meow-delete)
     '("e" . meow-line)
     '("E" . meow-kmacro-lines)
     '("f" . meow-find)
     '("g" . meow-keypad-start)
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
     '("O" . meow-block-expand)
     '("p" . meow-yank)
     '("q" . avy-goto-char-2)
     '("Q" . meow-goto-line)
     '("r" . meow-kill)
     '("s" . meow-change)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("v" . meow-visit)
     '("V" . meow-kmacro-matches)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-keypad-start)
     '("y" . kill-ring-save)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("&" . meow-query-replace)
     '("%" . meow-query-replace-regexp)
     '("'" . meow-join)
     '("/" . counsel-find-file)
     '("`" . repeat)
     '("<escape>" . meow-cancel)))

  ;; meow-setup is your custom function, see below
  (meow-setup)
  ;; If you want relative line number in NORMAL state(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing by hand.
  (meow-setup-indicator))

(leaf avy
  :straight t
  :require t
  :config
  (leaf ace-pinyin
    :straight t
    :require t
    :config
    (ace-pinyin-global-mode 1)))

(leaf which-key
  :straight t
  :require t)

(leaf which-key
  :straight t
  :require t
  :config
  (which-key-mode))

(leaf ace-window
  :straight t
  :require t
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(provide 'init-meow)
