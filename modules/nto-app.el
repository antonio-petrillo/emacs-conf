;; nto-app.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :ensure t
  :after latex
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode))

(provide 'nto-app)
