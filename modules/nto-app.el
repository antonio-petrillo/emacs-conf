;; nto-app.el -*- lexical-binding: t; -*-

(use-package pdf-tools
  :ensure t
  :after latex
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook 'auto-revert-mode))

(use-package buffer-to-pdf
  :ensure (:type git :host github :repo "protesilaos/buffer-to-pdf")
  :config
  (setq buffer-to-pdf-directory (expand-file-name "~/Downloads/")))

(provide 'nto-app)
