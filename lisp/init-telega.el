;;; -*- lexical-binding: t; -*-

(setq telega-use-images t
      telega-emoji-use-images nil
      telega-chat-input-markups '(nil "org" "markdown2")
      telega-open-file-function 'org-open-file
      telega-filter-button-width '(0.25 8 17)
      telega-server-libs-prefix "/opt/td/"
      telega-avatar-workaround-gaps-for '(return t)
      telega-filters-custom
      '(("Main" . main)
        ("Groups" . (type basicgroup supergroup))
        ("Channels" . (type channel))
        ("Archive" . archive)))

(with-eval-after-load "telega"
  (setq telega-chat-mode-hook nil))

(provide 'init-telega)
