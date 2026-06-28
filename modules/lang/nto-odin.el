;; nto-odin.el -*- lexical-binding: t; -*-

;; TODO: save buffer before run odinfmt
(defun nto--run-odinfmt (&optional path-to-odinfmt)
  "Run `odinfmt' on current buffer without save, if called in a mode different than `odin-mode' it does absolute nothing."
  (interactive)
  (when (eq major-mode 'odin-mode)
    (let* ((buffer (current-buffer))
           (filename (buffer-file-name buffer))
           (directory (directory-file-name (file-name-directory filename)))
           ;; TODO: accept optional arg for `odinfmt' path
           (odinfmt-cmd (format "%s -path:%s -w" "odinfmt" filename)))
      (progn
        (shell-command odinfmt-cmd buffer)
        (revert-buffer :noconfirm t)
        (message (concat "Formatting: " (file-name-nondirectory filename)))))))

(use-package odin-mode
  :ensure (:host github :repo "antonio-petrillo/odin-mode")
  :bind
  (:map odin-mode-map
        ("<localleader> f" . #'nto--run-odinfmt)))

(provide 'nto-odin)
