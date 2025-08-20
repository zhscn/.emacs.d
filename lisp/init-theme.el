;;; -*- lexical-binding: t -*-
(straight-use-package 'hl-todo)
(straight-use-package 'autothemer)
(straight-use-package 'minions)

(setq column-number-mode t)
(minions-mode)

(setq modus-themes-region '(bg-only no-extend))

(require 'kaolin-themes)
(setq kaolin-themes-italic-comments t
      kaolin-ocean-alt-bg t)

(if (display-graphic-p)
    (load-theme 'kaolin-light t)
  (load-theme 'modus-vivendi t))

(when window-system
  (set-fontset-font t 'unicode "Symbola" nil 'append)
  (set-face-attribute
   'default nil :font (font-spec :family "Sarasa Fixed SC" :size 14))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset (font-spec :family "Sarasa Fixed SC"))))

(global-hl-todo-mode +1)

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
  (let ((curr-file-name (if (eq (buffer-name) (car +smart-file-name-cache))
                            (cdr +smart-file-name-cache)
                          (let ((file-name (+smart-file-name)))
                            (setq +smart-file-name-cache (cons (buffer-name) file-name))
                            file-name))))
    (if (and (buffer-modified-p) (buffer-file-name))
        (concat "*" curr-file-name)
      curr-file-name)))

(defun +persp-name ()
  (when (length> persp-names-cache 1)
    (format "#%.5s " (safe-persp-name (get-current-persp)))))

(defun +format-mode-line ()
  (let* ((lhs '((:eval (meow-indicator))
                (:eval (+smart-file-name-cached))
                " %l,%C"))
         (rhs '((:eval (rime-lighter)) " "
                (:eval (+persp-name))
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

;; (advice-add 'meow-setup-indicator :around #'(lambda (_) (setq-default mode-line-format '((:eval (+format-mode-line))))))

(provide 'init-theme)
