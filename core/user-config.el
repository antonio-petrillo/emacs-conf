;; user-config.el -*- lexical-binding: t; -*-

(defun nto--load-user-config ()
  "Load the user config located at `nto--user-config' var."
  (load nto--user-config))

(unless (file-exists-p nto--user-config)
  (copy-file "user-config-template.el" nto--user-config nil))

(provide 'user-config)
