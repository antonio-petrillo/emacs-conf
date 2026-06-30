;; nto-dired.el -*- lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :after evil
  :commands (dired dired-jump)
  :custom
  (dired-auto-revert-buffer #'dired-buffer-stale-p)
  (dired-listing-switches "-aghi -v")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-create-destination-dirs 'ask)
  (dired-mouse-drag-files t)
  (dired-make-directory-clickable t)
  (dired-dwim-target t)
  :bind
  ((:map dired-mode-map
         ("SPC" . nil))) ;; free up space for <leader>

  :config
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-define-key 'normal dired-mode-map
    (kbd "h") #'dired-up-directory
    (kbd "l") #'dired-find-file))

(use-package dired-subtree
  :ensure t
  :bind
  (:map dired-mode-map
        ("<tab>" . #'dired-subtree-toggle)
        ("TAB" . #'dired-subtree-toggle)
        ("<backtab>" . #'dired-subtree-remove)
        ("S-TAB" . #'dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(provide 'nto-dired)
