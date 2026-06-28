;; emacs-builtin.el -*- lexical-binding: t; -*-

(defun nto--backward-kill-word()
  "Same as `backward-kill-word' but if it is invoked on a white space character
at the beginning of the line it will stop at it, furthermore if it is invoked
on the beginning of the line it will go the end of the previous line instead
of delete the previous word."
  (interactive)
  (let ((same? (save-excursion
                 (let ((orig (line-number-at-pos (point)))
                       (dest (progn
                               (backward-word)
                               (line-number-at-pos (point)))))
                   (eq orig dest))))
        (start? (eq (point) (line-beginning-position))))
    (cond (start? (backward-delete-char 1))
          (same? (backward-kill-word 1))
          (:else (kill-line 0)))))

(defun nto--keyboard-quit-dwim()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- If the evil state is not normal then set to normal
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((not (= 'normal evil-state)) (evil-normal-state))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))


(use-package emacs
  :ensure nil
  :after evil
  :custom
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (mode-line-format (delq 'mode-line-modes mode-line-format))
  (ibuffer-expert t)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Programming" (predicate . (derived-mode-p 'prog-mode)))
      ("Org" (mode . org-mode))
      ("MD" (mode . markdown-mode))
      ("Dired" (mode . dired-mode)))))
  (ibuffer-never-show-predicates
   '(;; System buffers
     "^\\*Messages\\*$"
     "^\\*scratch\\*$"
     "^\\*Completions\\*$"
     "^\\*Help\\*$"
     "^\\*Apropos\\*$"
     "^\\*info\\*$"
     "^\\*Async-native-compile-log\\*$"))
  (ibuffer-formats
   '((mark " " (name 60 -1 :left))))
  (repeat-on-final-keystroke t)
  (repeat-exit-timeout 3)
  (repeat-exit-key "<escape>")
  (repeat-keep-prefix nil)
  (repeat-check-key t)
  :init
  (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))
  (auto-revert-mode)
  (setq completions-highlight-face nil)

  (load-theme 'modus-operandi)

  (global-set-key [remap backward-kill-word] #'nto--backward-kill-word)
  (global-set-key [remap keyboard-quit] #'nto--keyboard-quit-dwim)

  (global-unset-key (kbd "C-z"))

  (add-hook 'java-mode-hook 'subword-mode)
  (add-hook 'prog-mode-hook (lambda ()
                              (display-line-numbers-mode 1)
			      (toggle-truncate-lines 1)
                              (setq-local display-line-numbers 'relative)))

  (recentf-mode 1)
  (repeat-mode 1)
  :bind
  ("C-S-i" . #'dabbrev-expand)
  ("<leader> ie" . #'emoji-list)
  ("<leader> ii" . #'emoji-insert)
  ("<leader> id" . #'emoji-describe)
  ("<leader> is" . #'emoji-search)
  ("<leader> ir" . #'emoji-recent)
  ("<leader> iu" . #'insert-char)
  ("<leader> fp" . (lambda () (interactive) (dired user-emacs-directory)))
  ("<leader> fP" . (lambda () (interactive) (find-file nto--user-config)))

  (:map emacs-lisp-mode-map
        ("<localleader> e" . #'eval-defun)
        ("<localleader> b" . #'eval-buffer)
        ("<localleader> r" . #'eval-region)))

(use-package eldoc
  :ensure nil
  :defer t
  :hook (prog-mode . eldoc-mode))

(use-package hide-show
  :ensure nil
  :init
  (setq hs-hide-comments t
	hs-hide-initial-comment-block t
	hs-isearch-open t)
  :hook (prog-mode . hs-minor-mode))

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init-hook . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" nto--cache)
        history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package winner
  :ensure nil
  :custom
  (winner-dont-bind-my-keys t)
  :hook (elpaca-after-init-hook . winner-mode)
  :bind
  (("<leader> wu" . #'winner-undo)
   ("<leader> wr" . #'winner-redo)))

(use-package delsel
  :ensure nil
  :hook (elpaca-after-init-hook . delete-selection-mode))

(use-package electric
  :ensure nil
  :hook ((prog-mode . electric-indent-mode)
	 (prog-mode . electric-pair-mode))
  :config
  (setq-default electric-indent-chars '(?\n ?\^?))
  (setq electric-pair-pairs '((?\{ . ?\})
                              (?\[ . ?\])
                              (?\( . ?\))
                              (?\" . ?\"))))
(use-package project
  :ensure nil
  :bind-keymap
  ("C-x p" . project-prefix-map)
  :bind
  (("<leader> pp" . #'project-switch-project)
   ("<leader> pb" . #'project-switch-to-buffer)
   ("<leader> pc" . #'project-compile)
   ("<leader> ps" . #'project-shell)
   ("<leader> pe" . #'project-eshell)
   ("<leader> pf" . #'project-find-file)
   ("<leader> pk" . #'project-kill-buffers)
   ("<leader> p&" . #'project-async-shell-command)))

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-separator "  "
        which-key-prefix-prefix "... "
        which-key-max-display-columns 2
        which-key-idle-delay 1.0
        which-key-idle-secondary-delay 0.25
        which-key-add-column-padding 1
        which-key-max-description-length 45))

(provide 'emacs-builtin)
