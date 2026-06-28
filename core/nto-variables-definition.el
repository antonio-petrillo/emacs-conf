;; nto-variables-definition.el -*- lexical-binding: t; -*-

(defvar nto--user-config
  (file-name-concat (getenv "HOME") ".local" "emacs" "user-config.el"))

(defvar nto--notes-dir-parent
  (file-name-concat (getenv "HOME") "Documents" "Notes"))

(defvar nto--notes-dir
  (expand-file-name "roam-denote-notes/" nto--notes-dir-parent))

(defvar nto--notes-unsorted-dir
  (expand-file-name "notes/" nto--notes-dir-parent))

(defvar nto--org-directory
  nto--notes-unsorted-dir)

(defvar nto--notes-assets-dir
  (expand-file-name "assets" nto--notes-dir-parent))

(provide 'nto-variables-definition)
