;; nto-completion-at-point.el -*- lexical-binding: t; -*-

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
	("C-i" . #'corfu-complete)
	("C-n" . #'corfu-next)
	("C-p" . #'corfu-previous)
	("RET" . #'corfu-insert)
	("C-q" . #'corfu-quick-complete)
	("M-C-SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-cycle-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay 0.5)
  (corfu-quit-no-match 'separator)

  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (corfu-echo-mode 1)

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  (add-hook 'corfu-mode-hook (lambda ()
                               (setq-local completion-sytle '(orderless basic)
                                           completion-category-overrides nil
                                           completion-category-defaults nil))))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'nto-completion-at-point)
