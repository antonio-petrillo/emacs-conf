;; nto-prog.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))

(require 'nto-lua)
(require 'nto-odin)
(require 'nto-elixir)
(require 'nto-go)

(use-package devdocs
  :ensure t
  :custom
  (devdocs-data-dir (expand-file-name "devdocs" nto--cache))
  :bind
  (("<leader> hd" . #'devdocs-lookup)))

(use-package dotenv-mode
  :defer t
  :ensure t)

(use-package editorconfig
  :ensure t
  :after ws-butler
  :custom
  (editorconfig-trim-whitespaces-mode #'ws-butler-mode)
  :config
  (add-to-list 'editorconfig-exclude-regexps
               "\\.\\(zip\\|\\(doc\\|xls\\|ppt\\)x\\)\\'")
  (setq editorconfig-get-properties-function #'editorconfig-get-properties)
  (editorconfig-mode 1))

(use-package ws-butler
  :ensure t
  :hook (prog-mode . ws-butler-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p
        trashed-use-header-line t
        trash-sort-key '("Date deleted: " . t)
        trashed-date-format "%d-%m-%Y %H:%M:%S"))

(provide 'nto-prog)
