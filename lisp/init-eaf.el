(straight-use-package 'deferred)
(straight-use-package 's)
;; (straight-use-package '(epc :type git :host github :repo "MatthewZMD/emacs-epc"))
(straight-use-package 'epc)
(straight-use-package '(ctable :type git :host github :repo "kiwanami/emacs-ctable"))
(add-to-list 'load-path "~/.emacs.d/emacs-application-framework")

(setq eaf-terminal-font-size "18")
(require 'eaf)

(provide 'init-eaf)
