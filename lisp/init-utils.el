;;; init-utils.el -*- lexical-binding: t -*-
;;; Commentary: some other settings
;;; Code:

(use-package multiple-cursors
  :bind (("s-<mouse-1>"   . mc/add-cursor-on-click)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hl-todo
  :bind (:map hl-todo-mode-map
              ([C-f3] . hl-todo-occur)
              ("C-c p" . hl-todo-previous)
              ("C-c n" . hl-todo-next)
              ("C-c o" . hl-todo-occur))
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix ""
        projectile-sort-order 'recentf
        projectile-use-git-grep t)
  :config
  ;; (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'.
  (when (and (not (executable-find "fd"))
             (executable-find "rg"))
    (setq projectile-generic-command
          (let ((rg-cmd ""))
            (dolist (dir projectile-globally-ignored-directories)
              (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
            (concat "rg -0 --files --color=never --hidden" rg-cmd))))

  ;; Support Perforce project
  (let ((val (or (getenv "P4CONFIG") ".p4config")))
    (add-to-list 'projectile-project-root-files-bottom-up val)))

;;(use-package flywrap
;;  :straight (flywrap
;;             :type git
;;             :host github
;;             :repo "casouri/lunarymacs"
;;             :files ("site-lisp/flywrap.el"))
;;  :hook (text-mode . #'flywrap-mode))

;;;; set gdb multi-windows when open
(setq gdb-many-windows t)

;;;; customize the gdb multi-windows
(defadvice gdb-setup-windows (after my-setup-gdb-windows activate)
  "my gdb UI"
  (gdb-get-buffer-create 'gdb-stack-buffer)
  (set-window-dedicated-p (selected-window) nil)
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)

  (let ((w-asm (selected-window))
        (w-source (split-window nil nil 'left)))

    (select-window w-source)
    (set-window-buffer
     w-source
     (if gud-last-last-frame
         (gud-find-file (car gud-last-last-frame))
       (if gdb-main-file
           (gud-find-file gdb-main-file)
         (list-buffers-noselect))))
    (setq gdb-source-window (selected-window))

    (let ((w-io (split-window nil (/ (* (window-height) 5) 6))))
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-inferior-io) nil w-io))
    (let ((w-stack (split-window nil (/ (* (window-height) 3) 4))))
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-stack-buffer) nil w-stack))

    (select-window w-asm)
    (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-disassembly-buffer))
    (let ((w-gdb (split-window nil (/ (* (window-height) 5) 6))))
      (set-window-buffer w-gdb gud-comint-buffer))
    (let ((w-local (split-window nil (/ (* (window-height) 1) 2))))
      (gdb-set-window-buffer (gdb-get-buffer-create 'gdb-locals-buffer) nil w-local))
    (other-window 1)
    (other-window 1)))

;;(set-window-dedicated-p mygdb-io t)
;;(set-window-buffer mygdb-io    (gdb-get-buffer-create 'gdb-inferior-io))
;;(set-window-buffer mygdb-local (gdb-get-buffer-create 'gdb-locals-buffer))
;;(set-window-buffer mygdb-stack (gdb-get-buffer-create 'gdb-stack-buffer))
;;(set-window-buffer mygdb-asm   (gdb-get-buffer-create 'gdb-disassembly-buffer))
;;(set-window-buffer mygdb-regs  (gdb-get-buffer-create 'gdb-registers-buffer))
;;(set-window-buffer mygdb-gdb    gud-comint-buffer)

(provide 'init-utils)

;;; init-utils.el ends here
