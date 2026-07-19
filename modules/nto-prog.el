;; nto-prog.el -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "modules/lang" user-emacs-directory))

(add-hook 'java-mode-hook 'subword-mode)
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)
			    (toggle-truncate-lines 1)
                            (setq-local display-line-numbers 'relative)))

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

(use-package ghostel
  :ensure t
  :if (member sytem-type '(gnu/linux darwin))
  :bind
  (("<leader> ot" . #'ghostel)))

(use-package ghostel-eshell
  :ensure nil
  :after ghostel
  :hook (eshell-load . ghostel-eshell-visual-command-mode))
(use-package ghostel-compile
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-compile-global-mode))
(use-package ghostel-comint
  :ensure nil
  :after ghostel
  :hook (after-init . ghostel-comint-global-mode))

(provide 'nto-prog)
