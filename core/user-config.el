;; user-config.el -*- lexical-binding: t; -*-

(defvar nto--user-config
  (file-name-concat (getenv "HOME") ".local" "emacs" "user-config.el"))

(defun load-user-config ()
  "Load the user config located at `nto--user-config' var."
  (load nto--user-config))

(when (not (file-exists-p nto--user-config))
  (copy-file "user-config-template.el" nto--user-config nil))

(add-hook 'elpaca-after-init-hook #'load-user-config)

(provide 'user-config)
