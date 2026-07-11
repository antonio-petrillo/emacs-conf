;; nto-org.el -*- lexical-binding: t; -*-

;; I love org-mode but it bind to much stuff
(use-package org
  :ensure nil
  :defer t
  :bind
  (("<leader> oa" . #'org-agenda)
   :map org-mode-map
   ("C-'" . nil)
   ("C-," . nil)
   ("M-;" . nil)
   ("M-l" . nil)
   ("C-c ;" . nil)
   ("<localleader> c" . #'org-toggle-checkbox)
   ("<localleader> st" . #'org-time-stamp)
   ("<localleader> ss" . #'org-schedule)
   ("<localleader> sd" . #'org-deadline)
   ("<localleader> t" . #'org-agenda-todo)
   ("<localleader> f" . #'org-footnote-new))
  :custom
  (org-directory nto--org-directory)
  (org-agenda-files (directory-files-recursively org-directory ".*journal.*\\.org$"))
  (org-agenda-span 'week)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-ellipsis "⮧")
  (org-adapt-indentation nil)
  (org-special-ctrl-a/e nil)
  (org-special-ctrl-k nil)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'current-window)
  (org-edit-src-persistent-message nil)
  (org-src-fontify-natively t)
  (org-src-preserve-indentation t)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers t)
  (org-edit-src-content-indentation 0)
  (org-export-with-toc t)
  (org-cycle-emulate-tab t)
  (org-export-headline-levels 8)
  :config
  (add-hook 'org-mode-hook #'visual-line-mode)
  (plist-put org-format-latex-options :scale 2.0))

(use-package org-modern
  :ensure t
  :defer t
  :after org
  :hook
  ((org-mode . org-modern-mode)
   (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-table nil)
  (org-modern-star nil)
  (org-modern-block-fringe nil))

(use-package org-appear
  :ensure t
  :defer t
  :hook
  ((org-mode . (lambda ()
                 (add-hook 'evil-insert-state-entry-hook
                           #'org-appear-manual-start nil t)
                 (add-hook 'evil-insert-state-exit-hook
                           #'org-appear-manual-stop nil t)))
   (org-mode . org-appear-mode)))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'nto-org)
