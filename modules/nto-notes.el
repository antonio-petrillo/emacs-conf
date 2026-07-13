;; nto-notes.el -*- lexical-binding: t; -*-

(use-package denote
  :ensure t
  :commands (denote-directory)
  :hook
  ((text-mode . denote-fontify-links-mode-maybe)
   (dired-mode . denote-dired-mode))
  :bind
  (("<leader> na" . #'nto--intern-assets)
   ("<leader> nr" . #'denote-rename-file-using-front-matter)
   ("<leader> nu" . #'nto--unsorted-note))

  :config
  (defun nto--unsorted-note ()
    (interactive)
    (let* ((title (denote-title-prompt nil))
           (keywords (denote-keywords-prompt))
           (extension (concat "." (completing-read "Extension: " '("typ" "tex" "org" "md" "txt"))))
           (id (format-time-string denote-date-identifier-format))
           (filename (denote-format-file-name nto--notes-unsorted-dir id keywords title (if (string= extension ".")) "")))
      (find-file filename)))

  (defun nto--dired-rename-using-denote-filescheme (input-path output-path &optional move-if-non-nil)
    (interactive)
    (let* ((filename (file-name-nondirectory input-path))
           (extension (file-name-extension filename t))

           (title (denote-title-prompt nil))
           (keywords (denote-keywords-prompt))
           (id (format-time-string denote-date-identifier-format))

           (new-path (denote-format-file-name
                      output-path
                      id keywords title extension "")))

      (when (or (not input-path) (not (file-regular-p input-path)))
        (user-error "The file to intern into notes asset must be a regular file"))

      (if move-if-non-nil
          (dired-rename-file input-path new-path nil)
        (dired-copy-file input-path new-path nil))
      (message (format "%s: %s to %s" (if move-if-non-nil "Moved" "Copied") input-path new-path))))

  (defun nto--dired-intern-assets (&optional move-if-non-nil)
    (interactive "P")
    (let ((input-path (dired-get-file-for-visit))
          (nto--dired-rename-using-denote-filescheme input-path nto--notes-assets-dir move-if-non-nil))))

  (defun nto--intern-assets (&optional move-if-non-nil)
    (interactive "P")
    (let ((filename (read-file-name "Select: ")))
      (if (and filename (file-regular-p filename))
          (nto--dired-rename-using-denote-filescheme filename nto--notes-assets-dir move-if-non-nil)
        (user-error "The file to intern into notes asset must be a regular file"))))

  (setq denote-directory nto--notes-dir)
  (setq denote-file-type 'org)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-buffer-name-prefix "[Note] ")
  (setq denote-rename-buffer-mode "%D")
  (denote-rename-buffer-mode 1))

(use-package denote-journal
    :ensure t
    :bind
    ("<leader> nj" . #'denote-journal-new-or-existing-entry)
    :custom
    (denote-journal-directory nto--journal-dir)
    (denote-journal-keyword "journal"))

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
  (("<leader> nn" . #'denote-roam-find-or-create-node)
   ("<leader> ni" . #'denote-roam-insert-or-create-node))
  :custom
  (denote-roam-include-journal t)
  (denote-roam-directory nto--notes-dir)
  :config
  (add-hook 'denote-after-rename-file-hook
            (lambda () (when (buffer-file-name)
                         (org-roam-db-update-file (buffer-file-name))
                         (message (format "update roam id for %s" (buffer-file-name))))))
  (denote-roam-mode t))

(with-eval-after-load 'transient
  (transient-define-prefix nto--notes-menu ()
    [["Visit"
      ("n" "Notes" (lambda () (interactive) (dired nto--notes-dir)))
      ("a" "Assets" (lambda () (interactive) (dired nto--notes-assets-dir)))
      ("j" "Journal" (lambda () (interactive) (dired nto--journal-dir)))
      ("u" "Unsorted" (lambda () (interactive) (dired nto--notes-unsorted-dir)))]
     ["Visualize"
      ("g" "Open Graph" org-roam-ui-open)
      ;; add denote journal visualization
      ]])


  (use-package emacs
    :ensure nil
    :bind
    ("<leader> nm" . #'nto--notes-menu)))

(provide 'nto-notes)
