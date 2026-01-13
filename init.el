;; init.el --- Builtin module -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" nto--cache))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(when (and (eq system-type 'windows-nt)
	   (not (eq (pwd) "c:/Program Files/Emacs")))
  (cd (getenv "HOME")))

(use-package exec-path-from-shell
  :if (eq system-type 'gnu/linux)
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; from prelude emacs
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

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
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
	evil-want-keybinding nil
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
  (evil-collection-init
   '(dired eww magit term wdired)))

(use-package evil-terminal-cursor-changer
  :if (not (eq system-type 'windows-nt))
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

(defvar nto--user-config
  (file-name-concat (getenv "HOME") ".local" "emacs" "user-config.el"))

(if (file-exists-p nto--user-config)
    (load nto--user-config)
  (progn
    (copy-file "user-config-template.el" nto--user-config nil)
    (load nto--user-config)))

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
   ("M-X" . #'consult-mode-command)

   ("<leader> ht" . #'load-theme)
   ("<leader> hi" . #'consult-info)
   ("<leader> hm" . #'consult-man)

   ("<leader> ss" . #'consult-line)
   ("<leader> so" . #'consult-outline)
   ("<leader> si" . #'consult-imenu)
   ("<leader> jc" . #'consult-line)
   ("<leader> jC" . #'goto-line)
   ("<leader> fg" . #'consult-ripgrep)
   ("<leader> /" . #'consult-ripgrep)
   ("<leader> ff" . #'consult-find)
   ("<leader> fl" . #'consult-locate)
   ("<leader> RET" . #'bookmark-jump)
   ("<leader> bb" . #'switch-to-buffer)

   ("<leader> xr" . #'consult-register)

   :map minibuffer-local-map
   ("M-s" . #'consult-history)
   ("M-r" . #'consult-history))
  :config
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref))

(use-package consult-dir
  :ensure t
  :defer t
  :bind
  (([remap list-directory] . #'consult-dir)
   ("<leader> fd" . #'consult-dir)
   (:map vertico-map
         ("C-x C-d" . #'consult-dir)
         ("C-x C-j" . #'consult-dir-jump-file))))

(eval-when-compile
  (defmacro nto--embark-ace-action (fn)
    `(defun ,(intern (concat "nto--embark-ace-" (symbol-name fn))) ()
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn))))))

  (defmacro nto--embark-split-action (fn split-type)
    `(defun ,(intern (concat "nto--embark-"
                             (symbol-name fn)
                             "-"
                             (car (last (split-string
                                         (symbol-name split-type) "-"))))) ()
       (interactive)
       (funcall #',split-type)
       (call-interactively #',fn)))

  (nto--embark-ace-action find-file)
  (nto--embark-ace-action switch-to-buffer)

  (nto--embark-split-action find-file split-window-below)
  (nto--embark-split-action find-file split-window-right)

  (nto--embark-split-action switch-to-buffer split-window-below)
  (nto--embark-split-action switch-to-buffer split-window-right))


(use-package embark
  :ensure t
  :bind
  (("C-c C-a" . #'embark-act)
   ("C-." . #'embark-act)
   ("C-," . #'embark-dwim)
   ("<leader> hE" . #'embark-bindings)
   (:map embark-file-map
         ("w" . #'nto--embark-ace-find-file)
         ("2" . #'nto--embark-find-file-below)
         ("3" . #'nto--embark-find-file-right))
   (:map embark-buffer-map
         ("w" . #'nto--embark-ace-switch-to-buffer)
         ("2" . #'nto--embark-switch-to-buffer-below)
         ("3" . #'nto--embark-switch-to-buffer-right))
   (:map minibuffer-local-map
         ("C-c C-c" . #'embark-collect)
         ("C-c C-e" . #'embark-export)
         ("C-c C-b" . #'embark-become)))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "M-[") #'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "M-]") #'better-jumper-jump-forward)))

(use-package dired
  :ensure nil
  :after evil
  :commands (dired)
  :custom
  (dired-listing-switches "-aghi -v")
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

(use-package trashed
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
  (setq denote-directory (file-name-concat nto--notes-dir "notes")
        denote-assets-directory (file-name-concat nto--notes-dir "assets"))
  (setq denote-file-type 'org)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[Note] ")
  (setq denote-rename-buffer-mode "%D")
  (denote-rename-buffer-mode 1))

(use-package denote-journal
  :ensure t
  :commands (denote-journal-new-entry
             denote-journal-new-or-existing-entry
             denote-journal-link-or-create-entry)
  :hook (calendar-mode . denote-journal-calendar-mode)
  :bind
  (("<leader> n j" . #'denote-journal-new-or-existing-entry)
   ("<leader> n J" . #'denote-journal-link-or-create-entry))
  :config
  (setq denote-journal-directory (expand-file-name "journal" denote-directory)
        denote-journal-keyword "journal"
        denote-journal-title-format 'day-date-month-year))

;; I love org-mode but it bind to much stuff
(use-package org
  :ensure nil
  :defer t
  :bind
  (("<leader> oa" . #'org-agenda)
   :map org-mode-map
   ("C-'" . nil)
   ("C-," . nil)
   ("M-;" . nil)
   ("M-l" . nil)
   ("C-c ;" . nil)
   ("<localleader> c" . #'org-toggle-checkbox)
   ("<localleader> st" . #'org-time-stamp)
   ("<localleader> ss" . #'org-schedule)
   ("<localleader> sd" . #'org-deadline)
   ("<localleader> t" . #'org-agenda-todo)
   ("<localleader> f" . #'org-footnote-new))
  :custom
  (org-agenda-files (directory-files-recursively org-directory ".*journal.*\\.org$"))
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-ellipsis "⮧")
  (org-adapt-indentation nil)
  (org-special-ctrl-a/e nil)
  (org-special-ctrl-k nil)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-persistent-message nil)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-edit-src-content-indentation 0)
  (org-export-with-toc t)
  (org-cycle-emulate-tab t)
  (org-export-headline-levels 8))

(use-package org-modern
  :ensure t
  :defer t
  :after org
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-table nil)
  (org-modern-star nil)
  (org-modern-block-fringe nil))

(use-package org-appear
  :ensure t
  :defer t
  :hook  
  ((org-mode . (lambda ()
                 (add-hook 'evil-insert-state-entry-hook
                           #'org-appear-manual-start nil t)
                 (add-hook 'evil-insert-state-exit-hook
                           #'org-appear-manual-stop nil t)))
   (org-mode . org-appear-mode)))

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

(use-package dotenv-mode
  :defer t
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defmacro nto--aas-expand-and-move (text n)
  `(lambda ()
     (interactive)
     (insert ,text)
     (backward-char ,n)))

(use-package aas
  :ensure t
  :hook
  ((org-mode . aas-activate-for-major-mode)
   (markdown-mode . aas-activate-for-major-mode)
   (latex-mode . aas-activate-for-major-mode))
  :config
  (aas-set-snippets 'markdown-mode
                    ";[" "[ ] "
                    ";b" (nto--aas-expand-and-move "**** " 3)
                    ";i" (nto--aas-expand-and-move "** " 2))
  (aas-set-snippets 'org-mode
                    ";[" "[ ] "
                    ";b" (nto--aas-expand-and-move "** " 2)
                    ";i" (nto--aas-expand-and-move "// " 2)
                    ";;4" (nto--aas-expand-and-move "$$$$ " 3)
                    ";4" (nto--aas-expand-and-move "$$ " 2)))

(use-package rotate-text
  :ensure (:host github :repo "debug-ito/rotate-text.el")
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
  :if (not (eq system-type 'windows-nt))
  :hook ((org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (text-mode . jinx-mode))
  :bind
  (([remap ispell-word] . #'jinx-correct)
   ("<leader> lc" . #'jinx-correct))
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
  (("<leader> lt" . #'google-translate-at-point)
   ("<leader> lT" . #'google-translate-at-point-reverse))
  :init
  (add-to-list 'display-buffer-alist
	       '("\\*Google Translate\\*"
		 (display-buffer-reuse-window
		  display-buffer-below-selected)
		 (dedicated . t)
		 (window-height . fit-window-to-buffer)
		 (body-function . (lambda (window) (select-window window))))))

(use-package powerthesaurus
  :ensure t
  :bind
  ("<leader> la" . #'powerthesaurus-lookup-antonyms-dwim)
  ("<leader> ld" . #'powerthesaurus-lookup-definitions-dwim)
  ("<leader> lp" . #'powerthesaurus-lookup-dwim)
  ("<leader> lr" . #'powerthesaurus-lookup-related-dwim)
  ("<leader> ls" . #'powerthesaurus-lookup-synonyms-dwim)
  ("<leader> lS" . #'powerthesaurus-lookup-sentences-dwim)
  ("<leader> lP" . #'powerthesaurus-transient))

(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))

  ;; NOTE: add here defaults fonts for others os'
  (pcase system-type
    ('gnu/linux 
     (progn
       (setq mono-spaced-font "IosevkaTerm Nerd Font Mono"
	     proportionately-spaced-font "Roboto Serif")))
    ('windows-nt (progn
		   (setq mono-spaced-font "Iosevka"
			 proportionately-spaced-font "Roboto"))))

  (set-face-attribute
   'default nil
   :family mono-spaced-font
   :height 180)

  (set-face-attribute
   'fixed-pitch nil
   :family mono-spaced-font
   :height 1.0)

  (set-face-attribute
   'variable-pitch nil
   :family proportionately-spaced-font
   :height 1.0))

(use-package devdocs
  :ensure t
  :custom
  (devdocs-data-dir (expand-file-name "devdocs" nto--cache))
  :bind
  (("<leader> hd" . #'devdocs-lookup)))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package editorconfig
  :ensure t
  :after ws-butler
  :custom
  (editorconfig-trim-whitespaces-mode #'ws-butler-mode)
  :config
  (editorconfig-mode 1))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :bind
  ("<leader> gg" . #'magit-status)
  ("<leader> gl" . #'magit-log)
  ("<leader> gb" . #'magit-branch)
  ("<leader> gf" . #'magit-fetch)
  ("<leader> gp" . #'magit-pull)
  ("<leader> gP" . #'magit-push)
  ("<leader> gc" . #'magit-commit))

(use-package git-timemachine
  :ensure t
  :defer t
  :bind
  ("<leader> gt" . #'git-timemachine-toggle)
  :config
  (evil-define-key 'normal 'git-timemachine-mode-map
    (kbd "C-p") #'git-timemachine-show-previous-revision
    (kbd "C-n") #'git-timemachine-show-next-revision
    (kbd "gb")  #'git-timemachine-blame
    (kbd "gtc") #'git-timemachine-show-commit))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter))

(use-package go-mode
  :ensure t)

(use-package odin-mode
  :ensure (:host sourcehut :repo "mgmarlow/odin-mode")
  :bind
  (:map odin-mode-map
        ("<localleader> c" . #'odin-build-project)
        ("<localleader> C" . #'odin-check-project)
        ("<localleader> r" . #'odin-run-project)
        ("<localleader> t" . #'odin-test-project)))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-odin
  :ensure (:host github :repo "mattt-b/flycheck-odin")
  :hook (flycheck-mode . flycheck-odin-setup))

(use-package elixir-mode
  :ensure t
  :bind
  (:map elixir-mode-map
        ("<localleader> f" . #'elixir-format)))

(use-package lua-mode
  :ensure t)
