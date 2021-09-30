;;; -*- lexical-binding: t; -*-
(straight-use-package '(a :type git :host github :repo "plexus/a.el"))
(leaf clojure-mode
  :straight t)
(leaf cider
  :straight t
  :commands cider
  :init
  (setq
   cider-font-lock-dynamically nil
   cider-font-lock-reader-conditionals t
   cider-use-fringe-indicators t
   cider-prompt-for-symbol nil
   cider-save-file-on-load t
   cider-enhanced-cljs-completion-p t
   cider-offer-to-open-cljs-app-in-browser nil)
  :config
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
      (cider-browse-spec spec)))
  :bind
  ((:cider-stacktrace-mode-map
    ("n" . next-line)
    ("p" . previous-line)
    ("P" . cider-stacktrace-show-only-project))
   (:cider-mode-map
    ("C-c M-s" . +clojure-describe-spec)
    ("C-c f" . cider-pprint-eval-defun-at-point)
    ("C-c C-f" . nil))
   (:clojure-mode-map
    (("C-c C-i" . cider-inspect-last-result)))))
(provide 'init-clojure)
