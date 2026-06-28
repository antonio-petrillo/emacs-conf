;; nto-snippet.el -*- lexical-binding: t; -*-

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
   (typst-ts-mode . aas-activate-for-major-mode)
   (latex-mode . aas-activate-for-major-mode))
  :config
  (aas-set-snippets 'markdown-mode
                    ";[" "[ ] "
                    ";b" (nto--aas-expand-and-move "****" 2)
                    ";i" (nto--aas-expand-and-move "**" 1))
  (aas-set-snippets 'typst-ts-mode
                    ";b" (nto--aas-expand-and-move "**" 1)
                    ";i" (nto--aas-expand-and-move "__" 1)
                    ";;4" (nto--aas-expand-and-move "$  $" 2)
                    ";4" (nto--aas-expand-and-move "$$" 1))
  (aas-set-snippets 'org-mode
                    ";[" "[ ] "
                    ";b" (nto--aas-expand-and-move "**" 1)
                    ";i" (nto--aas-expand-and-move "//" 1)
                    ";;4" (nto--aas-expand-and-move "$$$$" 2)
                    ";4" (nto--aas-expand-and-move "$$" 1)))

(provide 'nto-snippet)
