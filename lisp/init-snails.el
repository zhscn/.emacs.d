(straight-use-package '(snails :type git :host github :repo "manateelazycat/snails"))

(require 'snails)
(global-set-key (kbd "M-[") 'snails)
;(setq snails-show-with-frame nil)
(snails '(snails-backend-buffer snails-backend-recentf snails-backend-imenu snails-backend-current-buffer snails-backend-rg snails-backend-fd snails-backend-fasd snails-backend-command) t)
(add-to-list 'meow-mode-state-list '(snails-mode . insert))
(provide 'init-snails)
