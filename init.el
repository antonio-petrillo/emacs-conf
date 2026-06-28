;; init.el --- Initialization file for Emacs -*- lexical-binding: t; -*-
;;; Commentary: Emacs Startup File --- initialization for Emacs

(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(require 'nto-core)
(require 'nto-prog)
(require 'nto-git)
(require 'nto-completion-at-point)
(require 'nto-consult)
(require 'nto-dired)
(require 'nto-writing)
(require 'nto-snippet)
(require 'nto-move-around)
(require 'nto-org)
(require 'nto-markdown)
(require 'nto-typst)
(require 'nto-latex)
(require 'nto-app)
(require 'nto-ui)
(require 'nto-notes)
