;;; -*- lexical-binding: t -*-

(setq tramp-default-method "ssh"
      tramp-use-ssh-controlmaster-options nil
      directory-abbrev-alist '(("^/cri" . "/ssh:crimson:")))
(setq tramp-verbose 10)
(provide 'init-tramp)
