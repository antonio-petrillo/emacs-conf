;; nto-latex.el -*- lexical-binding: t; -*-

(use-package auctex
  :ensure t
  :init
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t)
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        reftex-plug-into-AUCTeX t)

  (setq-default TeX-master nil)

  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex))

(use-package cdlatex
  :ensure t
  :after latex
  :hook ((LaTeX-mode . cdlatex-mode)
         (Latex-Mode . cdlatex-electricindex-mode))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(use-package adaptive-wrap
  :ensure t
  :hook (Latex-Mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))

(defun nto--latex-live-preview ()
  (interactive)
  (auctex-cont-latexmk-mode)
  (TeX-command-sequence t t))

(use-package auctex-cont-latexmk
  :ensure t)

(use-package evil-tex
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode))

(provide 'nto-latex)
