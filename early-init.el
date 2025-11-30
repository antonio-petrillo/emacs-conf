;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;
;;; Code:

(defvar nto--cache
  (file-name-concat (getenv "HOME") ".local" "emacs"))

(unless (file-exists-p nto--cache)
  (make-directory nto--cache t))

(setq eshell-directory-name (file-name-concat nto--cache "eshell")
      treesit-extra-load-path `(,(file-name-concat nto--cache "tree-sitter")))

(setq
 auto-revert-verbose nil
 auto-save-list-file-prefix (file-name-concat nto--cache "auto-save-list/.saves-")
 auto-save-default nil
 auto-window-vscroll nil
 bookmark-default-file (file-name-concat nto--cache "bookmarks")
 column-number-mode t
 create-lockfiles nil
 custom-file (file-name-concat nto--cache "custom.el")
 custom-safe-theme t
 default-input-method "italian-postfix"
 delete-by-moving-to-trash t
 delete-selection-mode 1
 display-line-numbers-type 'relative
 global-auto-revert-non-file-buffers t
 hscroll-margin 2
 hscroll-step 1
 inhibit-startup-buffer-menu t
 inhibit-startup-message t
 inhibit-startup-screen t
 ispell-dictionary "en_US,it_IT"
 package-user-dir (file-name-concat nto--cache "elpa")
 package-enable-at-startup nil
 project-list-file (file-name-concat nto--cache "projects")
 make-backup-files nil
 mouse-wheel-scroll-amount '(2 ((shift) . hscroll)
                               (mouse-wheel-scroll-amount-horizontal 2))
 org-directory (file-name-concat (getenv "HOME") "Documents" "Org")
 org-persist-directory temporary-file-directory
 pixel-scroll-precision-mode t
 pixel-scroll-precision-use-momentum nil
 read-answer-short t
 recentf-save-file (file-name-concat nto--cache "recentf")
 ring-bell-function 'ignore
 scroll-conservatively  10
 scroll-margin 0
 scroll-preserve-screen-position t
 tab-always-indent 'complete
 tab-width 4
 transient-history-file (file-name-concat nto--cache "transient/history.el")
 treesit-font-lock-level 4
 truncate-lines t
 use-dialog-box nil
 use-short-answer t
 warning-minimum-level :emergency)

(startup-redirect-eln-cache (file-name-concat nto--cache "eln-cache"))

(load custom-file :no-error-if-file-is-missing)

(add-to-list 'default-frame-alist
	     '(menu-bar-lines . 0))

(add-to-list 'default-frame-alist
	     '(tool-bar-lines . 0))

(add-to-list 'default-frame-alist
	     '(vertical-scrool-bars . 0))

(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent
	native-compile-prune-cache t))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar nto--file-name-handler-alist file-name-handler-alist)
(defvar nto--vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (setq gc-cons-threshold (* 16 1024 1024)
		  gc-cons-percentage 0.1
		  file-name-handler-alist nto--file-name-handler-alist
		  vc-handled-backends nto--vc-handled-backends)))
