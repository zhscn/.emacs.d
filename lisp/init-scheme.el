;;; init-scheme.el -*- lexical-binding: t -*-
;;; Commentary: scheme
;;; Code:

(use-package geiser
  :init
  (setq geiser-active-implementations '(chez guile chicken mit chibi))
  (setq geiser-mode-start-repl-p t)
  :config
  (defun chez/delete-so (&optional directory)
    (interactive
     (list (or (and current-prefix-arg
                    (read-directory-name "Run in directory: " nil nil t))
               default-directory)))
    (when (buffer-file-name)
      (let* ((command (or (and (boundp 'executable-command) executable-command)
                          (concat "rm *.so")))
             (default-directory directory)
             (compilation-ask-about-save nil))
        (shell-command (read-shell-command "Run: " command))))))

(provide 'init-scheme)

;;; init-scheme.el ends here
