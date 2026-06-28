;; nto-move-around.el -*- lexical-binding: t; -*-

(use-package better-jumper
  :ensure t
  :after evil
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "M-[") #'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "M-]") #'better-jumper-jump-forward)))

(use-package avy
  :ensure t
  :after evil
  :config
  (setq avy-all-windows nil)
  :bind
  (("<leader> jj" . #'avy-goto-char-timer)
   ("<leader> jl" . #'avy-goto-line)
   ("<leader> je" . #'avy-goto-end-of-line)
   ("<leader> jw" . #'avy-goto-word-0)))

(use-package ace-window
  :ensure t
  :after evil
  :bind
  (("<leader> ww" . #'ace-window)
   ("<leader> wS" . #'ace-swap-window)
   ("<leader> w C-w" . #'ace-swap-window)
   ("<leader> wx" . #'ace-delete-window))
  :config
  (setq aw-background nil)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?c aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'."))

(provide 'nto-move-around)
