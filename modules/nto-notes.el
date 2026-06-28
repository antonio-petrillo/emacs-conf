;; nto-notes.el -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :commands (denote-directory)
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  (("<leader> nr" . #'denote-rename-file-using-front-matter)
   ("<leader> nu" . #'nto--unsorted-note))
  :preface
  (defun nto--unsorted-note ()
    (interactive)
    (let* ((title (denote-title-prompt nil))
           (keywords (denote-keywords-prompt))
           (extension (format ".%s" (completing-read "Extension: " '("typ" "tex" "org" "md" "txt"))))
           (id (format-time-string denote-date-identifier-format))
           (filename (denote-format-file-name nto--notes-unsorted-dir id keywords title extension "")))
      (find-file filename)))

  :config
  (setq denote-directory nto--notes-dir)
  (setq denote-file-type 'org)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[Note] ")
  (setq denote-rename-buffer-mode "%D")
  (denote-rename-buffer-mode 1))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory nto--notes-dir)
  (org-roam-db-location (file-name-concat nto--cache "org-roam.db"))
  :bind
  (("<leader> nl" . #'org-roam-buffer-toggle))
  :config
  (org-roam-db-autosync-mode 1))

(use-package websocket
  :ensure t)

(use-package compat
  :ensure t)

(use-package org-roam-ui
  :ensure t
  :after (org-roam websocket compat)
  :custom
  (org-roam-ui-open-on-start nil)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start nil))

(use-package denote-roam
  :ensure (:type git :host github :repo "BardofSprites/denote-roam")
  :bind
  ("<leader> nn" . #'denote-roam-find-or-create-node)
  ("<leader> ni" . #'denote-roam-insert-or-create-node)
  :custom
  (denote-roam-include-journal nil)
  (denote-roam-directory nto--notes-dir)
  :config
  (add-hook 'denote-after-rename-file-hook
            (lambda () (when (buffer-file-name)
                         (org-roam-db-update-file (buffer-file-name)))))
  (denote-roam-mode t))

(provide 'nto-notes)
