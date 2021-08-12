;;; -*- lexical-binding: t -*-

(leaf ivy
  :straight t
  :require t
  :hook (after-init-hook . ivy-mode)
  :init

  (setq ivy-height 12
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ivy-read-action-function #'ivy-hydra-read-action
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t
        ivy-initial-inputs-alist nil)

  :config
  (leaf counsel
    :straight t
    :require t)
  (leaf swiper
    :straight t
    :require t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )


;; (leaf selectrum
;;   :straight t
;;   :require t
;;   :config
;;   (leaf selectrum-prescient
;;     :straight t
;;     :require t
;;     :config
;;     (selectrum-prescient-mode t))
;;   (selectrum-mode t)
;;
;;   (defun +minibuffer-backward-delete ()
;;     (interactive)
;;     (delete-region
;;      (or
;;       (save-mark-and-excursion
;;         (while (equal ?/ (char-before)) (backward-char))
;;         (when-let ((p (re-search-backward "/" (line-beginning-position) t)))
;;           (1+ p)))
;;       (save-mark-and-excursion (backward-word) (point)))
;;      (point)))
;;   :bind
;;   ((:selectrum-minibuffer-map
;;     ("M-DEL" . +minibuffer-backward-delete))))

(provide 'init-complete)
