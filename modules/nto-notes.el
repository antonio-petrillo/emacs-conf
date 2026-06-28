;; nto-notes.el -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :commands (denote-directory)
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  (("<leader> nn" . #'denote)
   ("<leader> nN" . #'denote-type)
   ("<leader> nd" . #'nto--denote-dired)
   ("<leader> ni" . #'denote-link)
   ("<leader> nb" . #'denote-backlinks)
   ("<leader> nr" . #'denote-rename-file-using-front-matter))
  :init
  (defun nto--denote-dired ()
    (interactive)
    (dired (denote-directory)))
  :config
  (defvar denote-assets-directory (file-name-concat nto--notes-dir "assets"))
  (setq denote-directory nto--notes-dir)
  (setq denote-file-type 'org)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[Note] ")
  (setq denote-rename-buffer-mode "%D")
  (denote-rename-buffer-mode 1))

(with-eval-after-load 'denote
  (defun nto--denote-format-string-for-org-front-matter (s)
    (format "%S" s))

  (defun nto--denote-trim-whitespace-and-double-quote (s)
    (let ((s (denote-trim-whitespace s)))
          (string-trim s "\"" "\"")))

  (defvar nto--denote-typst-front-matter
    "#set document(
	title: %s,
	date: %s,
	keywords: %s,
	description: %s,
)

")

  (defun nto--denote-format-keywords-for-typst-front-matter (keywords)
    (if keywords
        (format "(%s)"
                (mapconcat
                 (lambda (keyword) (format "%S" keyword))
                 keywords
                 ", "))
      ""))

  (defun nto--denote-extract-keywords-from-front-matter (keywords-string)
    (split-string keywords-string "[:,\s]+" t "[][ \"'()]+"))

  (defvar nto--denote-typst-link-format
    "#link(denote:%s)[%s]")

  (defvar nto--denote-typst-link-in-context-regexp
    (concat "#link(denote:"
            "\\(?1:[^][]*?\\)"
            ")\\["
            "\\(?2:.*?\\)"
            "]"))

  (defun nto--denote-typst-datetime (date)
    (if date
        (format-time-string "datetime(day: %d, month: %m, year: %Y, hour: %H, minute: %M, second: %S)" date)))

  (defun nto--denote-extract-date-from-front-matter-typst (date-string)
    (let ((date-string (denote-trim-whitespace date-string)))
      (if (string-empty-p date-string)
          nil
        (when (string-match
               "datetime(day: \\([0-9]+\\), month: \\([0-9]+\\), year: \\([0-9]+\\), hour: \\([0-9]+\\), minute: \\([0-9]+\\), second: \\([0-9]+\\))"
               date-string)
          (date-to-time
           (format "%s-%s-%sT%s:%s:%s"
                   (match-string 3 date-string)
                   (match-string 2 date-string)
                   (match-string 1 date-string)
                   (match-string 4 date-string)
                   (match-string 5 date-string)
                   (match-string 6 date-string)))))))

  ;; NOTE: signature is not supported
  (add-to-list 'denote-file-types
               '(typst
                 :extension ".typ"
                 :front-matter nto--denote-typst-front-matter
                 :title-key-regexp "^\\stitle\\s-*:"
                 :title-value-function nto--denote-format-string-for-org-front-matter
                 :title-value-reverse-function nto--denote-trim-whitespace

                 :keywords-key-regexp "^#\\s-+keywords:"
                 :keywords-value-function nto--denote-format-keywords-for-typst-front-matter
                 :keywords-value-reverse-function nto--denote-extract-keywords-from-front-matter

                 :identifier-key-regexp "^#\\s-+description:"
                 :identifier-value-function nto--denote-format-string-for-org-front-matter
                 :identifier-value-reverse-function nto--denote-trim-whitespace

                 :date-key-regexp "^\\s-+datetime("
                 :date-value-function nto--denote-typst-datetime
                 :date-value-reverse-function nto--denote-extract-date-from-front-matter-typst

                 :link-retrieval-format "#link(denote:%VALUE%)"
                 :link nto--denote-typst-link-format
                 :link-in-context-regexp nto--denote-typst-link-in-context-regexp)))

(use-package consult-denote
  :ensure t
  :after denote
  :bind
  (("<leader> nf" . #'consult-denote-find)
   ("<leader> ng" . #'consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package denote-markdown
  :ensure t
  :after denote
  :commands (denote-markdown-convert-links-to-file-paths
             denote-markdown-convert-links-to-denote-type
             denote-markdown-convert-links-to-obsidian-type
             denote-markdown-convert-obsidian-links-to-denote-type))

(provide 'nto-notes)
