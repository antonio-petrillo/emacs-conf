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
  (dired-mouse-drag-files t)
  (dired-make-directory-clickable t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-dwim-target t)
  :bind
  ((:map dired-mode-map
         ("C-c C-e" . #'wdired-change-to-wdired-mode)))
  :config
  (evil-set-initial-state 'dired-mode 'motion)
  (evil-define-key 'motion dired-mode-map
    (kbd "+") #'dired-create-directory
    (kbd "h") #'dired-up-directory
    (kbd "l") #'dired-find-file))

(with-eval-after-load 'transient
  (transient-define-prefix nto--dired-menu ()
    [["Visit"
      ("j" "Downloads" (lambda () (interactive) (dired "~/Downloads")))
      ("d" "Documents" (lambda () (interactive) (dired "~/Documents/")))
      ("i" "Images" (lambda () (interactive) (dired "~/Pictures/")))
      ("v" "Videos" (lambda () (interactive) (dired "~/Videos/")))
      ("p" "Projects" (lambda () (interactive) (dired "~/Projects/")))
      ("c" "Config" (lambda () (interactive) (dired "~/.config/")))]])

  (use-package emacs
    :ensure nil
    :bind
    ("<leader> fm" . #'nto--dired-menu)))

(use-package dired-x
  :ensure nil
  :after dired
  :defer t
  :hook (dired-mode . dired-omit-mode)
  :bind
  ((:map dired-mode-map
         ("C-h" . #'dired-omit-mode)))
  :config
  ;; Taken from Doom Emacs
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\..+$"
                "\\|^\\.DS_Store\\'"
                "\\|^flycheck_.*"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-clean-confirm-killing-deleted-buffers nil))

(use-package dired-aux
  :ensure nil
  :after dired
  :defer t
  :custom
  (dired-isearch-filenames 'dwim)
  (dired-vc-rename-file t)
  (dired-create-destination-dirs 'ask))

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

(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

(provide 'nto-dired)
