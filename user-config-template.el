;; user-config.el --- Builtin module -*- lexical-binding: t; -*-
(let ((mono-spaced-font "Monospace")
      (proportionately-spaced-font "Sans"))

  ;; NOTE: add here defaults fonts for others os'
  (pcase system-type
    ('gnu/linux
     (progn
       (setq mono-spaced-font "Iosevka Term"
	     proportionately-spaced-font "Roboto Serif")))
    ('windows-nt (progn
		   (setq mono-spaced-font "Iosevka"
			 proportionately-spaced-font "Roboto"))))

  (set-face-attribute
   'default nil
   :family mono-spaced-font
   :height 180)

  (set-face-attribute
   'fixed-pitch nil
   :family mono-spaced-font
   :height 1.0)

  (set-face-attribute
   'variable-pitch nil
   :family proportionately-spaced-font
   :height 1.0))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-zenburn))

(use-package ef-themes
  :ensure t)

(use-package doric-themes
  :ensure t)

(use-package year-1984-theme
  :ensure t)

(use-package anti-zenburn-theme
  :ensure t)

(use-package green-is-the-new-black-theme
  :ensure t)

(provide 'user-config)
