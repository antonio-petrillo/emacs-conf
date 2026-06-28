;; nto-typst.el -*- lexical-binding: t; -*-

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :bind
  (:map typst-ts-mode-map
        ("M-S-h" . #'typst-ts-mode-meta-decrease)
        ("M-S-l" . #'typst-ts-mode-meta-increase)
        ("M-S-k" . #'typst-ts-mode-meta-up)
        ("M-S-j" . #'typst-ts-mode-meta-down)
        ("<localleader> c" . #'typst-ts-compile)
        ("<localleader> w" . #'typst-ts-watch-mode)
        ("<localleader> p" . #'typst-ts-preview)
        ("<localleader> P" . #'typst-ts-compile-and-preview)))

(with-eval-after-load 'jinx
  (with-eval-after-load 'typst-ts-mode
    (add-to-list
     'jinx-exclude-faces
     '(typst-ts-mode
       font-lock-warning-face font-lock-function-name-face font-lock-function-call-face
       font-lock-variable-name-face font-lock-variable-use-face font-lock-keyword-face
       font-lock-comment-delimiter-face font-lock-type-face font-lock-constant-face
       font-lock-builtin-face font-lock-preprocessor-face
       font-lock-negation-char-face font-lock-escape-face font-lock-number-face
       font-lock-operator-face font-lock-property-use-face font-lock-punctuation-face
       font-lock-bracket-face font-lock-delimiter-face font-lock-misc-punctuation-face
       typst-ts-markup-item-indicator-face typst-ts-markup-term-indicator-face
       typst-ts-markup-rawspan-indicator-face typst-ts-markup-rawspan-blob-face
       typst-ts-markup-rawblock-indicator-face typst-ts-markup-rawblock-lang-face
       typst-ts-markup-rawblock-blob-face
       typst-ts-error-face typst-ts-shorthand-face typst-ts-markup-linebreak-face
       typst-ts-markup-quote-face typst-ts-markup-url-face typst-ts-math-indicator-face))))

(with-eval-after-load 'eglot
  (with-eval-after-load 'typst-ts-mode
    (add-to-list 'eglot-server-programs
                 `((typst-ts-mode) .
                   ,(eglot-alternatives `(,typst-ts-lsp-download-path "tinymist" "typst-lsp"))))))

(provide 'nto-typst)
