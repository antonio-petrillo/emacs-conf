;; text-goodies.el --- text-goodies config -*- lexical-binding: t; -*-
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rotate-text
  :ensure (:host github :repo "debug-ito/rotate-text.el")
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "]r") #'rotate-text
    (kbd "[r") #'rotate-text-backward))

(with-eval-after-load 'rotate-text
  (dolist (rotate-words '(("false" "true")
                          ("nord" "east" "sud" "ovest")
                          ("up" "down" "left" "right")
                          ("top" "bottom")))
    (cl-pushnew rotate-words rotate-text-words))
  (dolist (rotate-symbols '(("var" "const")
                            ("and" "or")
                            ("&&" "||")))
    (cl-pushnew rotate-symbols rotate-text-symbols)))

(use-package jinx
  :hook ((org-mode . jinx-mode)
         (markdown-mode . jinx-mode)
         (text-mode . jinx-mode))
  :bind
  (([remap ispell-word] . #'jinx-correct)
   ("<leader> lc" . #'jinx-correct)
   ("<leader> ll" . #'jinx-languages)
   ("<leader> ln" . #'jinx-next)
   ("<leader> lp" . #'jinx-previous))
  :config
  (setopt jinx-languages "en_US,it_IT"))

(use-package google-translate
  :ensure t
  :custom
  (google-translate-translation-directions-alist
        '(("it" . "en") ("en" . "it")))
  (google-translate-default-source-language "it")
  (google-translate-default-target-language "en")
  (google-translate-translation-to-kill-ring t)
  :bind
  (("<leader> lt" . google-translate-at-point)
   ("<leader> lT" . google-translate-at-point-reverse))
  :init
  (add-to-list 'display-buffer-alist
	       '("\\*Google Translate\\*"
		 (display-buffer-reuse-window
		  display-buffer-below-selected)
		 (dedicated . t)
		 (window-height . fit-window-to-buffer)
		 (body-function . (lambda (window) (select-window window))))))

(use-package tempel
  :bind
  (("M-=" . #'tempel-insert))
  :custom
  (tempel-trigger-prefix "<")
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

(provide 'text-goodies)
;; `text-goodies' ends here
