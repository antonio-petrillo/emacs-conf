;; minibuffer-setup.el -*- lexical-binding: t; -*-

(defun nto--take-me-home ()
  (interactive)
  (if (looking-back "/" nil)
      (progn
	(call-interactively #'delete-minibuffer-contents)
	(insert "~/"))
    (call-interactively #'self-insert-command)))

(use-package vertico
  :ensure t
  :hook (elpaca-after-init-hook . vertico-mode)
  :config
  (setq vertico-scrool-margin 0
	vertico-count 10
	vertico-resize t
	vertico-cycle t)
  :bind
  (:map vertico-map
	("C-a" . #'nto--take-me-home)
	("DEL" . #'vertico-directory-delete-char)
	("C-DEL" . #'vertico-directory-delete-word)))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :hook (vertico-mode . vertico-mouse-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic partial-completion))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (elpaca-after-init-hook . marginalia-mode)
  :bind
  (:map minibuffer-local-map
	("M-A" . #'marginalia-cycle)))

(provide 'minibuffer-setup)
