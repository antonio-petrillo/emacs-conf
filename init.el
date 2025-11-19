;; init.el --- Builtin module -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("elpa" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(use-package undo-tree
  :defer t
  :ensure t
  :hook
  (after-init . global-undo-tree-mode)
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
  :hook (after-init-hook . evil-mode)
  :custom
  (evil-disable-insert-state-bindings t)
  :init
  (setq evil-undo-system 'undo-tree
        evil-want-C-i-jumpt t
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-Y-yank-to-eol t
        evil-split-window-below t
        evil-split-window-right t
        evil-want-fine-undo t
        evil-toggle-key (kbd "C-q C-q C-z"))
  :config
  (keymap-unset evil-normal-state-map "C-." 'remove)
  (keymap-unset evil-normal-state-map "M-." 'remove)

  (evil-set-leader nil (kbd "M-SPC"))
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-set-leader nil (kbd "<leader> m") t)

  (evil-define-key '(normal visual operator replace motion) 'global
    (kbd "gr") #'revert-buffer)

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

    (kbd "<leader> bk") #'kill-current-buffer
    (kbd "<leader> bK") #'kill-buffer
    (kbd "<leader> br") #'revert-buffer

    (kbd "<leader> bm") #'bookmark-set
    (kbd "<leader> bd") #'bookmark-delete

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

(use-package evil-terminal-emulator-changer
  :ensure t
  :after evil
  :init
  (evil-terminal-cursor-changer-activate))

(use-package evil-escape
  :ensure t
  :after evil
  :init
  (evil-escape-mode)
  :config
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2))

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
  :init
  (auto-revert-mode)
  (setq completions-highlight-face nil)

  (global-set-key [remap backward-kill-word] #'nto--backward-kill-word)
  (global-set-key [remap keyboard-quit] #'nto--keyboard-quit-dwim)

  (global-unset-key (kbd "C-z"))

  (add-hook 'prog-mode-hook (lambda ()
                              (display-line-numbers-mode 1)
                              (hs-minor-mode)
                              (setq-local display-line-numbers 'relative)))

  (load-theme 'modus-vivendi)
  (recentf-mode)
  :bind
  ("<leader> ie" . #'emoji-list)
  ("<leader> ii" . #'emoji-insert)
  ("<leader> id" . #'emoji-describe)
  ("<leader> is" . #'emoji-search)
  ("<leader> ir" . #'emoji-recent)
  ("<leader> iu" . #'insert-char)

  (:map emacs-lisp-mode-map
        ("<localleader> e" . #'eval-defun)
        ("<localleader> b" . #'eval-buffer)
        ("<localleader> r" . #'eval-region)))

(use-package savehist
  :ensure nil
  :hook (after-init-hook . savehist-mode)
  :config
  (setq savehist-file (expand-file-name "savehist" nto--cache)
        history-length 100
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

(use-package winner
  :ensure nil
  :hook (after-init-hook . winner-mode)
  :bind
  (("<leader> wu" . #'winner-undo)
   ("<leader> wr" . #'winner-redo)))

(use-package delsel
  :ensure nil
  :hook (after-init-hook . delete-selection-mode))
                        
(use-package electric
  :ensure nil
  :hook (prog-mode . electric-indent-mode)
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

(use-package consult
  :ensure t
  :after evil
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  (([remap Info-search] . #'consult-info)
   ([remap switch-to-buffer] . #'consult-buffer)
   ([remap bookmark-jump] . #'consult-bookmark)
   ([remap load-theme] . #'consult-theme)
   ([remap project-switch-to-buffer] . #'consult-project-buffer)
   ([remap recentf-open-files] . #'consult-recent-file)
   ([remap evil-paste-pop] . #'consult-yank-pop) ;; shadowed by evil -_-
   ([remap yank-pop] . #'consult-yank-pop) ;; shadowed by evil -_-
   ([remap imenu] . #'consult-imenu)
   ([remap locate] . #'consult-locate)
   ([remap goto-line] . #'consult-goto-line)

   ("M-y" . #'evil-paste-pop)

   ("<leader> ht" . #'load-theme)
   ("<leader> hi" . #'consult-info)
   ("<leader> hm" . #'consult-man)

   ("<leader> ss" . #'consult-line)
   ("<leader> jc" . #'consult-line)
   ("<leader> jC" . #'goto-line)
   ("<leader> fg" . #'consult-ripgrep)
   ("<leader> /" . #'consult-ripgrep)
   ("<leader> ff" . #'consult-find)
   ("<leader> fl" . #'consult-locate)
   ("<leader> RET" . #'bookmark-jump)
   ("<leader> bb" . #'switch-to-buffer)

   :map minibuffer-local-map
   ("M-s" . #'consult-history)
   ("M-r" . #'consult-history)))

(use-package consult-dir
  :ensure t
  :defer t
  :bind
  (([remap list-directory] . #'consult-dir)
   ("<leader> fd" . #'consult-dir)
   (:map vertico-map
         ("C-x C-d" . #'consult-dir)
         ("C-x C-j" . #'consult-dir-jump-file))))

(use-package vertico
  :ensure t
  :hook (after-init-hook . vertico-mode)
  :config
  (setq vertico-scrool-margin 0
	vertico-count 10
	vertico-resize t
	vertico-cycle t)
  :bind
  (:map vertico-map
	("DEL" . #'vertico-directory-delete-char)
	("C-DEL" . #'vertico-directory-delete-word)))

(use-package vertico-mouse
  :ensure nil
  :after vertico
  :hook (vertico-mode . vertico-mouse-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :hook (after-init-hook . vertico-mode)
  :bind
  (:map minibuffer-local-map
	("M-A" . #'marginalia-cycle)))

(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("<tab>" . #'corfu-complete)
        ("C-n" . #'corfu-next)
        ("C-p" . #'corfu-previous)
        ("RET" . #'corfu-insert)
        ("C-q" . #'corfu-quick-complete))
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
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dict))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t 
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package better-jumper
  :ensure t
  :after evil
  :hook (evil-mode . better-jumper-mode)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-back] #'better-jumper-jump-backward)
  (global-set-key [remap xref-go-forward] #'better-jumper-jump-forward))

(use-package dired
  :ensure nil
  :after evil
  :commands (dired)
  :custom
  (dired-listing-switches "-aghi -v --group--directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'alwasy)
  (dired-mouse-drag-files t)
  (dired-make-directory-clickable t)
  (dired-dwim-target t)
  :bind
  (:map dired-mode-map
        ("SPC" . nil)) ;; free up space for <leader>
  :config
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

(use-package trahsed
  :ensure t
  :commands (trashed)
  :bind ("<leader> C-," . #'trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trash-sort-key '("Date deleted: " . t)
        trashed-date-format "%d-%m-%Y %H:%M:%S"))

(use-package darkroom
  :ensure t
  :bind
  (("<leader> tw" . #'darkroom-tentative-mode)))

(use-package markdown-mode
  :ensure t
  :custom
  (markdown-fontify-code-blocks-natively t))

(use-package denote
  :ensure t
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode)
   (markdown-mode . denote-dired-mode))
  :bind
  (("<leader> n n" . #'denote)
   ("<leader> n f" . #'denote-open-or-create)
   ("<leader> n N" . #'denote-type)
   ("<leader> n d" . #'denote-sort-dired)
   ("<leader> n i" . #'denote-link)
   ("<leader> n I" . #'denote-add-links)
   ("<leader> n b" . #'denote-backlinks)
   ("<leader> n r" . #'denote-rename-file-using-front-matter))
  :config
  (setq denote-directory (file-name-concat (getenv "HOME") "Documents" "Notes" "notes")
        denote-assets-directory (file-name-concat (getenv "HOME") "Documents" "Notes" "assets"))
  (setq denote-file-type 'markdown-yaml) ;; markdown is more *portable*
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[Note] ")
  (setq denote-rename-buffer-mode "%D")
  (denote-rename-buffer-mode 1))

;; I love org-mode but it bind to much stuff
(use-package org
  :ensure nil
  :bind
  (("<leader> oa" . #'org-agenda)
   :map org-mode-map
   ("C-'" . nil)
   ("C-," . nil)
   ("M-;" . nil)
   ("M-l" . nil)
   ("C-c ;" . nil))
  :config
  (setq org-agenda-span 'week
        org-agenda-start-on-weekday 1
        org-agenda-window-setup 'current-window)

  (setq org-M-RET-may-split-line '((default . nil))
        org-insert-heading-respect-content t
        org-log-done 'time
        org-log-into-drawer t
        org-ellipsis "⮧"
        org-adapt-indentation nil
        org-special-ctrl-a/e nil
        org-special-ctrl-k nil
        org-confirm-babel-evaluate nil
        org-src-window-setup 'current-window
        org-edit-src-persistent-message nil
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-hide-emphasis-markers t
        org-edit-src-content-indentation 0
        org-export-with-toc t
        org-cycle-emulate-tab t
        org-export-headline-levels 8))

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

(use-package devdocs
  :ensure t
  :custom
  (devdocs-data-dir (expand-file-name "devdocs" nto--cache))
  :bind
  (("<leader> hd" . #'devdocs-lookup)))

(use-package go-mode
  :ensure t)

(use-package odin-mode
  :ensure t
  :defer t
  :vc (odin-mode :url "https://git.sr.ht/~mgmarlow/odin-mode"
                 :branch "main")
  :bind
  (:map odin-mode-map
        ("<localleader> c" . #'odin-build-project)
        ("<localleader> C" . #'odin-check-project)
        ("<localleader> r" . #'odin-run-project)
        ("<localleader> t" . #'odin-test-project)))

(use-package elixir-ts-mode
  :ensure t)

(use-package elixir-mode
  :ensure t
  :bind
  (:map elixir-mode-map
        ("<localleader> f" . #'elixir-format)))

(use-package dotenv-mode
  :defer t
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defmacro nto--aas-expand-and-move (text n)
  `(progn
     (insert ,text)
     (backward-char ,n)))

(use-package aas
  :ensuret t
  :hook
  ((org-mode . aas-activate-for-major-mode)
   (markdown-mode . aas-activate-for-major-mode)
   (latex-mode . aas-activate-for-major-mode))
  :config
  (aas-set-snippets 'markdown-mode
    ";b" (nto--aas-expand-and-move "**** " 3)
    ";/" (nto--aas-expand-and-move "** " 2))
  (aas-set-snippets 'org-mode
    "mbb" (nto--aas-expand-and-move "\\mathbb{}" 1)
    "mca" (nto--aas-expand-and-move "\\mathcal{}" 1)
    ";ra" "\\rightarrow "
    ";la" "\\leftarrow "
    "__" (nto--aas-expand-and-move "_{}" 1)
    "^^" (nto--aas-expand-and-move "^{}" 1)
    "_sum" (nto--aas-expand-and-move "\\sum_{}" 1)
    "^sum" (nto--aas-expand-and-move "\\sum_{}^{}" 4)
    "_int" (nto--aas-expand-and-move "\\int_{}" 1)
    "^int" (nto--aas-expand-and-move "\\int_{}^{}" 4)
    ";b" (nto--aas-expand-and-move "** " 2)
    ";/" (nto--aas-expand-and-move "// " 2)
    ";A" "\\forall"
    ";E" "\\exists"
    ";|" "\\lor"
    ";&" "\\land"
    ";a" "\\alpha"
    ";;b" "\\beta"
    ";c" "\\gamma"
    ";d" "\\delta"
    ";e" "\\eta"
    ";E" "\\Eta"
    ";m" "\\mu"
    ";n" "\\nu"
    ";f" "\\phi"
    ";;f" "\\varphi"
    ";g" "\\nabla"
    ";s" "\\sigma"
    ";S" "\\Sigma"
    ";x" "\\times"
    ";." "\\cdot"
    ";;." "\\cdots"
    ";;$" (nto--aas-expand-and-move "$$$$ " 3)
    ";;4" (nto--aas-expand-and-move "$$$$ " 3)
    ";$" (nto--aas-expand-and-move "$$ " 2)
    ";4" (nto--aas-expand-and-move "$$ " 2)
    ";On" "O(n)"
    ";Oa" "O(1)"))

(use-package rotate-text
  :ensure t
  :vc (:url "https://github.com/debug-ito/rotate-text.el")
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "]r") #'rotate-text
    (kbd "[r") #'rotate-text-backward))

(with-eval-after-load 'rotate-text
  (dolist (rotate-words '(("false" "true")
                          ("nord" "east" "sud" "ovest")
                          ("up" "down" "left" "right")
                          ("top" "bottom")))
    (cl-pushnew rotate-words rotate-text-words))
  (dolist (rotate-symbols '(("var" "const")
                            ("and" "or")
                            ("&&" "||")))
    (cl-pushnew rotate-symbols rotate-text-symbols)))

(use-package jinx
  :ensure t
  :hook ((org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (text-mode . jinx-mode))
  :bind
  (([remap ispell-word] . #'jinx-correct)
   ("<leader> lc" . #'jinx-correct)
   ("<leader> ll" . #'jinx-languages)
   ("<leader> ln" . #'jinx-next)
   ("<leader> lp" . #'jinx-previous))
  :config
  (setopt jinx-languages "en_US,it_IT"))

(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (google-translate-default-source-language "it")
  (google-translate-default-target-language "en")
  (google-translate-translation-to-kill-ring t)
  :bind
  (("<leader> lt" . google-translate-at-point)
   ("<leader> lT" . google-translate-at-point-reverse))
  :init
  (add-to-list 'display-buffer-alist
	       '("\\*Google Translate\\*"
		 (display-buffer-reuse-window
		  display-buffer-below-selected)
		 (dedicated . t)
		 (window-height . fit-window-to-buffer)
		 (body-function . (lambda (window) (select-window window))))))

(let ((mono-spaced-font "Aporetic Serif Mono") ;; "Monospace" backup
      (proportionately-spaced-font "Aporetic Sans")) ;; "Sans" backup

  (set-face-attribute
   'default nil
   :family mono-spaced-font
   :height 120)

  (set-face-attribute
   'fixed-pitch nil
   :family mono-spaced-font
   :height 1.0)

  (set-face-attribute
   'variable-pitch nil
   :family proportionately-spaced-font
   :height 1.0))

(use-package doom-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(use-package ef-themes
  :ensure t)

(use-package tron-legacy-theme
  :ensure t)

(use-package spacemacs-theme
  :ensure t)

(use-package anti-zenburn-theme
  :ensure t)

(use-package gruber-darker-theme
  :ensure t)

(use-package naysayer-theme
  :ensure t)
