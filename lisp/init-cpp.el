;;; -*- lexical-binding: t -*-

(leaf global-tags
  :straight (global-tags :type git :host nil :repo "https://git.launchpad.net/global-tags.el")
  :require t
  :hook ((c-mode-hook c++-mode-hook) . global-tags-shared-backend-mode)
  :config
  (setq xref-search-program 'ripgrep)
  ;; xref (finding definitions, references)
  (add-to-list 'xref-backend-functions 'global-tags-xref-backend)
  ;; project.el (finding files)
  (add-to-list 'project-find-functions 'global-tags-try-project-root)
  ;; configure Imenu
  (add-hook 'ruby-mode-hook #'global-tags-imenu-mode)
  ;; to update database after save
  (add-hook 'c++-mode-hook (lambda ()
                             (add-hook 'after-save-hook
                                       #'global-tags-update-database-with-buffer
                                       nil
                                       t)))

)
(provide 'init-cpp)
