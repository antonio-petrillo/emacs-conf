;; nto-git.el -*- lexical-binding: t; -*-

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :bind
  ("<leader> gg" . #'magit-status)
  ("<leader> gl" . #'magit-log)
  ("<leader> gb" . #'magit-branch)
  ("<leader> gf" . #'magit-fetch)
  ("<leader> gp" . #'magit-pull)
  ("<leader> gP" . #'magit-push)
  ("<leader> gc" . #'magit-commit))

(use-package git-timemachine
  :ensure t
  :defer t
  :bind
  ("<leader> gt" . #'git-timemachine-toggle)
  :config
  (evil-define-key 'normal 'git-timemachine-mode-map
    (kbd "C-p") #'git-timemachine-show-previous-revision
    (kbd "C-n") #'git-timemachine-show-next-revision
    (kbd "gb")  #'git-timemachine-blame
    (kbd "gtc") #'git-timemachine-show-commit))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter))

(use-package git-modes
  :ensure t)

(provide 'nto-git)
