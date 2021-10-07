;;; -*- lexical-binding: t; -*-
(straight-use-package 'windmove)
(straight-use-package 'ace-window)
(straight-use-package 'avy)

(autoload #'ace-window "ace-window" nil t)
(define-key global-map (kbd "C-x C-o") #'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-alist
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

(autoload #'avy-goto-char-2 "avy" nil t)

(defun toggle-maximize-window ()
  "Maximize window."
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))

(provide 'init-window)
