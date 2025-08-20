;;; -*- lexical-binding: t; -*-
(leaf clojure-mode
  :straight t)
(leaf cider
  :straight t
  :require t
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
  (setq
   cider-font-lock-dynamically nil
   cider-font-lock-reader-conditionals t
   cider-use-fringe-indicators t
   cider-prompt-for-symbol nil
   cider-save-file-on-load t
   cider-enhanced-cljs-completion-p t
   cider-offer-to-open-cljs-app-in-browser nil)
  (define-key cider-stacktrace-mode-map (kbd "n") #'next-line)
  (define-key cider-stacktrace-mode-map (kbd "p") #'previous-line)
  (define-key cider-stacktrace-mode-map (kbd "P") #'cider-stacktrace-show-only-project)
  (define-key cider-mode-map (kbd "C-c M-s") #'+clojure-describe-spec)
  (define-key cider-mode-map (kbd "C-c f") #'cider-pprint-eval-defun-at-point)
  (define-key cider-mode-map (kbd "C-c C-f") 'nil)
  (define-key clojure-mode-map (kbd "C-c C-i") 'cider-inspect-last-result))
(provide 'init-clojure)
