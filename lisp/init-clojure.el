;;; -*- lexical-binding: t; -*-
(straight-use-package '(a :type git :host github :repo "plexus/a.el"))
(straight-use-package 'clojure-mode)
(straight-use-package 'cider)

(autoload #'cider "cider" nil t)

(setq
 cider-font-lock-dynamically nil
 cider-font-lock-reader-conditionals t
 cider-use-fringe-indicators t
 cider-prompt-for-symbol nil
 cider-save-file-on-load t
 cider-enhanced-cljs-completion-p t
 cider-offer-to-open-cljs-app-in-browser nil)

(with-eval-after-load "cider"
  (defun +clojure-describe-spec ()
    (interactive)
    (when-let* ((code (thing-at-point 'symbol))
                (dict (cider-nrepl-sync-request:eval
                       code
                       (--find (eq (cider-connection-type-for-buffer)
                                   (cider-connection-type-for-buffer it))
                               (cider-connections))
                       (cider-ns-from-form (cider-ns-form))))
                (spec (-last-item dict)))
      (cider-browse-spec spec))))

(provide 'init-clojure)
