;; nto-core.el -*- lexical-binding: t; -*-

(require 'nto-variables-definition)
(require 'elpaca-setup)

(require 'user-config)
(nto--load-user-config)

(require 'os-setup)
(require 'evil-setup)
(require 'emacs-builtin)
(require 'minibuffer-setup)

(provide 'nto-core)
