;; user-config.el -*- lexical-binding: t; -*-

(defun load-user-config ()
  "Load the user config located at `nto--user-config' var."
  (load nto--user-config))

(unless (file-exists-p nto--user-config)
  (copy-file "user-config-template.el" nto--user-config nil))

(add-hook 'elpaca-after-init-hook #'load-user-config)

(provide 'user-config)
