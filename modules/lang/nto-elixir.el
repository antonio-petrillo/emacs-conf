;; nto-elixir.el -*- lexical-binding: t; -*-

(use-package elixir-mode
  :ensure t
  :bind
  (:map elixir-mode-map
        ("<localleader> f" . #'elixir-format)))

(provide 'nto-elixir)
