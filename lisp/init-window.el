;;; -*- lexical-binding: t; -*-

(leaf windmove
  :straight t
  :require t)

(leaf ace-window
  :straight t
  :require t
  :bind
  ("C-x C-o" . ace-window)
  :config
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
	(?? aw-show-dispatch-help))))

(leaf avy
  :straight t
  :require t
  :config
  (leaf ace-pinyin
    :straight t
    :require t
    :config
    (ace-pinyin-global-mode 1)))

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
