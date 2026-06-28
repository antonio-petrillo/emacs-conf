;; nto-lua.el -*- lexical-binding: t; -*-

(use-package lua-mode
  :ensure t)

(use-package eglot-lua
  :ensure (:host github :repo "juergenhoetzel/eglot-lua")
  :after eglot
  :custom
  (eglot-lua-server-install-dir (file-name-concat nto--cache "EmmyLua-LanguageServer")))

(provide 'nto-lua)
