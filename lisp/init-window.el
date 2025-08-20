;;; -*- lexical-binding: t; -*-

(keymap-set global-map "C-x C-o" #'ace-window)
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

(provide 'init-window)
