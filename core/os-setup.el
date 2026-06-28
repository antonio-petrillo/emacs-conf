;; os-setup.el -*- lexical-binding: t; -*-

(when (and (eq system-type 'windows-nt)
	   (not (eq (pwd) "c:/Program Files/Emacs")))
  (cd (getenv "HOME")))

(use-package exec-path-from-shell
  :if (not (eq system-type 'windows-nt))
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; from prelude emacs
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic))))

(provide 'os-setup)
