;;; init-tex.el -*- lexical-binding: t -*-
;;; Commentary: LaTex config
;;; Code:

(add-to-list 'exec-path "/usr/local/texlive/2019/bin/x86_64-linux")
; (setq +latex-viewers '(zathura))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer) ;; revert pdf after compile
;(setq TeX-view-program-selection '((output-pdf "zathuraPDF Tools"))) ;; use pdf-tools for viewing
(setq TeX-view-program-selection '((output-pdf "zathura"))) ;; use pdf-tools for viewing
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq LaTeX-command "xelatex --synctex=1") ;; optional: enable synctex
;(setq pdf-latex-command "xelatex")
(setq pdf-latex-command "pdflatex")
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
            (setq TeX-command-extra-options "-file-line-error -shell-escape"
                  TeX-command-default "XeLaTeX"
                  TeX-auto-untabify t
                  TeX-engine 'xetex
                  TeX-show-compilation t)
            (TeX-global-PDF-mode t)
            (setq TeX-save-query nil)))

; (setq latex-enable-folding t
;       latex-enable-magic t
;       TeX-view-program-selection '((output-pdf "PDF Tools"))
;       TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))

(provide 'init-tex)

;;; init-tex.el ends here
