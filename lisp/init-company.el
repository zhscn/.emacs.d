;;; -*- lexical-binding: t -*-

(leaf company
  :straight t
  :require t
  :config
  (global-company-mode 1)
  :bind
  ((:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   (:company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)))
  :config
  (add-hook 'company-mode-hook #'(lambda ()
                                   (setq company-backends (delete 'company-clang company-backends))))
  (leaf company-posframe
    :straight t
    :require t
    :config
    (company-posframe-mode 1)))

(leaf awesome-pair
  :straight (awesome-pair :type git :host github :repo "manateelazycat/awesome-pair")
  :require t
  :config
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'java-mode-hook
                 'haskell-mode-hook
                 'maxima-mode-hook
                 'ielm-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'php-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'go-mode-hook
                 'qml-mode-hook
                 'jade-mode-hook
                 'css-mode-hook
                 'ruby-mode-hook
                 'coffee-mode-hook
                 'rust-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'swift-mode-hook
                 'minibuffer-inactive-mode-hook
                 ))
    (add-hook hook #'(lambda () (awesome-pair-mode 1))))
  :bind
  ((:awesome-pair-mode-map
    ("(" . awesome-pair-open-round)
    ("[" . awesome-pair-open-bracket)
    ("{" . awesome-pair-open-curly)
    (")" . awesome-pair-close-round)
    ("]" . awesome-pair-close-bracket)
    ("}" . awesome-pair-close-curly)
    ("=" . awesome-pair-equal)

    ("%" . awesome-pair-match-paren)
    ("\"" . awesome-pair-double-quote)

    ("SPC" . awesome-pair-space)

    ("M-O" . awesome-pair-backward-delete)
    ("C-d" . awesome-pair-forward-delete)
    ("C-k" . awesome-pair-kill)

    ("M-\"" . awesome-pair-wrap-double-quote)
    ("M-[" . awesome-pair-wrap-bracket)
    ("M-{" . awesome-pair-wrap-curly)
    ("M-(" . awesome-pair-wrap-round)
    ("M-)" . awesome-pair-unwrap)

    ("M-p" . awesome-pair-jump-right)
    ("M-n" . awesome-pair-jump-left)
    ("M-:" . awesome-pair-jump-out-pair-and-newline))))

(provide 'init-company)
