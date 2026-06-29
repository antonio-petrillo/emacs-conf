;; evil-setup.el -*- lexical-binding: t; -*-

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (elpaca-after-init-hook . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  :config
  (setq undo-tree-history-directory-alist `(("." . ,(file-name-concat nto--cache ".cache" "undo")))))

(use-package evil
  :ensure t
  :hook (elpaca-after-init-hook . evil-mode)
  :custom
  (evil-disable-insert-state-bindings t)
  :init
  (setq evil-undo-system 'undo-tree
        evil-want-C-i-jumpt t
        evil-want-C-u-scroll nil
        evil-want-C-d-scroll nil
	evil-want-keybinding nil
        evil-want-Y-yank-to-eol t
        evil-split-window-below t
        evil-split-window-right t
        evil-want-fine-undo t
        evil-toggle-key (kbd "C-q C-q C-z"))
  :config
  (evil-set-leader nil (kbd "M-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-set-leader nil (kbd "<leader> m") t)

  (evil-define-key '(normal visual operator replace motion) 'global
    (kbd "gr") #'revert-buffer)

  (define-key evil-insert-state-map (kbd "C-k") #'dabbrev-expand)
  (define-key evil-insert-state-map (kbd "C-n") nil)
  (define-key evil-insert-state-map (kbd "C-p") nil)
  (define-key evil-insert-state-map (kbd "C-q") nil)

  (evil-define-key '(insert normal) 'global
    (kbd "C-a") #'beginning-of-line
    (kbd "C-e") #'end-of-line
    (kbd "C-f") #'forward-char
    (kbd "C-b") #'backward-char
    (kbd "C-n") #'next-line
    (kbd "C-p") #'previous-line)

  (evil-define-key nil 'global
    (kbd "<leader> hk") #'describe-key
    (kbd "<leader> hv") #'describe-variable
    (kbd "<leader> hf") #'describe-function
    (kbd "<leader> hc") #'describe-command
    (kbd "<leader> hK") #'describe-keymap

    (kbd "<leader> .") #'find-file
    (kbd "<leader> ,") #'switch-to-buffer
    (kbd "<leader> fs") #'save-buffer
    (kbd "<leader> fr") #'recentf-open-files
    (kbd "<leader> fd") #'delete-file
    (kbd "<leader> fR") #'rename-file

    (kbd "<leader> bk") #'kill-current-buffer
    (kbd "<leader> bK") #'kill-buffer
    (kbd "<leader> br") #'revert-buffer
    (kbd "<leader> bi") #'ibuffer
    (kbd "<leader> bn") #'evil-buffer-new

    (kbd "<leader> bm") #'bookmark-set
    (kbd "<leader> bd") #'bookmark-delete
    (kbd "<leader> bs") #'bookmark-save

    (kbd "<leader> SPC") #'execute-extended-command
    (kbd "<leader> M-SPC") #'execute-extended-command-for-buffer

    (kbd "<leader> tl") #'display-line-numbers-mode
    (kbd "<leader> tt") #'toggle-truncate-lines

    (kbd "<leader> ws") #'evil-window-split
    (kbd "<leader> wv") #'evil-window-vsplit
    (kbd "<leader> wc") #'evil-window-delete
    (kbd "<leader> wm") #'toggle-frame-maximized
    (kbd "<leader> wh") #'evil-window-left
    (kbd "<leader> wj") #'evil-window-down
    (kbd "<leader> wk") #'evil-window-up
    (kbd "<leader> wl") #'evil-window-right
    (kbd "<leader> w1") #'delete-other-windows
    (kbd "<leader> wM") #'delete-other-windows
    (kbd "<leader> w0") #'delete-window
    (kbd "<leader> wo") #'other-window))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init '(calendar magit dired calc ediff)))

(use-package evil-terminal-cursor-changer
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :after evil
  :init
  (evil-terminal-cursor-changer-activate))

(use-package evil-escape
  :ensure t
  :after evil
  :hook (elpaca-after-init-hook . evil-escape-mode)
  :custom
  (evil-escape-key-sequence "jk")
  (evil-escape-delay 0.2)
  (evil-escape-excluded-states '(normal visual multiedit emacs motion)))

(use-package evil-exchange
  :ensure t
  :after evil
  :commands evil-exchange
  :init
  (evil-exchange-install))

(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :ensure t
  :after evil
  :commands evil-snipe-local-mode evil-snipe-override-local-mode
  :hook ((evil-mode . evil-snipe-override-mode)
         (evil-mode . evil-snipe-mode))
  :init
  (evil-define-key 'motion evil-snipe-override-local-mode-map
    "t" nil
    "T" nil)

  (setq evil-snipe-smart-case t
        evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t))

(use-package evil-visualstar
  :ensure t
  :after evil
  :hook (evil-mode . global-evil-visualstar-mode)
  :config
  (setq-default evil-visualstar/persistent t))

(use-package exato
  :ensure t
  :after evil)

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :init
  (evil-define-key '(visual normal) 'global "gc" #'evilnc-comment-operator))

(use-package evil-multiedit
  :ensure t
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "M-a") #'evil-multiedit-match-symbol-and-next
    (kbd "M-A") #'evil-multiedit-match-symbol-and-prev)

  (evil-define-key 'visual 'global
    (kbd "R") #'evil-multiedit-match-all
    (kbd "M-a") #'evil-multiedit-match-and-next
    (kbd "M-A") #'evil-multiedit-match-and-prev)

  (evil-define-key '(visual normal) 'global
    (kbd "C-M-a") #'evil-multiedit-restore)

  (with-eval-after-load 'evil-mutliedit

    (evil-define-key 'multiedit 'global
      (kbd "M-a")   #'evil-multiedit-match-and-next
      (kbd "M-S-a") #'evil-multiedit-match-and-prev
      (kbd "RET")   #'evil-multiedit-toggle-or-restrict-region)
    (evil-define-key '(multiedit multiedit-insert) 'global
      (kbd "C-h")   #'nto--backward-kill-word
      (kbd "C-n")   #'evil-multiedit-next
      (kbd "C-p")   #'evil-multiedit-prev))

  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-traces
  :ensure t
  :after evil
  :config
  (evil-traces-use-diff-faces)
  (evil-traces-mode))

(provide 'evil-setup)
