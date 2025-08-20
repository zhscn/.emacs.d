;;; -*- lexical-binding: t; -*-
(straight-use-package 'windmove)
(straight-use-package 'ace-window)
(straight-use-package 'avy)
(straight-use-package 'persp-mode)

(define-key global-map (kbd "C-x C-o") #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-dispatch-alist
      '((?x aw-delete-window "Delete Window")
	    (?m aw-swap-window "Swap Windows")
	    (?M aw-move-window "Move Window")
	    (?c aw-copy-window "Copy Window")
	    (?n aw-switch-buffer-in-window "Select Buffer")
	    (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	    (?c aw-split-window-fair "Split Fair Window")
	    (?v aw-split-window-vert "Split Vert Window")
	    (?b aw-split-window-horz "Split Horz Window")
	    (?o delete-other-windows "Delete Other Windows")
	    (?? aw-show-dispatch-help)))

(defun toggle-maximize-window ()
  "Maximize window."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(require 'persp-mode)
;; (set-persp-parameter 'dont-save-to-file t nil)
(setq-default persp-auto-save-opt 0)
(persp-mode +1)


(provide 'init-window)
