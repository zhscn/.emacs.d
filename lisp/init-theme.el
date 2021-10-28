;;; -*- lexical-binding: t -*-
(straight-use-package 'hl-todo)

(setq modus-themes-region '(bg-only no-extend))
(load-theme 'modus-vivendi t)

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(global-hl-todo-mode +1)

(setq-default header-line-format nil)

(defvar +smart-file-name-cache nil)

(defun +smart-file-name ()
  "Get current file name, if we are in project, the return relative path to the project root, otherwise return absolute file path.
This function is slow, so we have to use cache."
  (let ((vc-dir (vc-root-dir))
        (bfn (buffer-file-name (current-buffer))))
    (cond
     ((and bfn vc-dir)
      (concat
       (file-name-base (string-trim-right vc-dir "/")) "/" (file-relative-name bfn vc-dir)))
     (bfn bfn)
     (t (buffer-name)))))

(defun +smart-file-name-cached ()
  (if (eq (buffer-name) (car +smart-file-name-cache))
      (cdr +smart-file-name-cache)
    (let ((file-name (+smart-file-name)))
      (setq +smart-file-name-cache
            (cons (buffer-name) file-name))
      file-name)))

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                "%l,%C "
                (:eval (+smart-file-name-cached))))
         (rhs '((:eval persp-last-persp-name)
                " "
                (:eval (rime-lighter))
                (:eval mode-name)
                (vc-mode vc-mode)))
         (ww (window-width))
         (lhs-str (format-mode-line lhs))
         (rhs-str (format-mode-line rhs))
         (rhs-w (string-width rhs-str)))
    (format "%s%s%s"
            lhs-str
            (propertize " " 'display `((space :align-to (- (+ right right-fringe right-margin) (+ 1 ,rhs-w)))))
            rhs-str)))
(advice-add 'meow-setup-indicator :around #'(lambda (_) (setq-default mode-line-format '((:eval (+format-mode-line))))))

(provide 'init-theme)
