;;; init-ui.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil :slant 'italic)

(custom-set-faces
 '(default
   ((t
     (:family
      "XD"
      :foundry "CYEL"
      :slant normal
      :weight semi-bold
      :height 157
      :width normal)))))

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; disable startup screen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...\n")

;; nice scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; there's a built-in linum-mode, but we're using
;; display-line-numbers-mode or nlinum-mode,
;; as it's supposedly faster
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
  (global-nlinum-mode t))

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Lolo - "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

;; icons
(use-package all-the-icons :if (display-graphic-p))

;; themes
(use-package gruber-darker-theme :ensure t)

(use-package nano-theme)
(use-package nano-modeline)
(use-package nano-agenda)

(when lolo-theme
  (load-theme lolo-theme t))

;; dashboard
(use-package
 dashboard
 :demand
 :diminish (dashboard-mode page-break-lines-mode)
 :bind
 (:map
  dashboard-mode-map
  (("n" . dashboard-next-line)
   ("p" . dashboard-previous-line)
   ("N" . dashboard-next-section)
   ("F" . dashboard-previous-section)))
 :custom (dashboard-banner-logo-title "")
 (dashboard-startup-banner
  (expand-file-name "banner.txt" user-emacs-directory))
 (dashboard-items '((recents . 7) (bookmarks . 7) (agenda . 5)))
 (initial-buffer-choice
  (lambda () (get-buffer dashboard-buffer-name)))
 (dashboard-set-heading-icons t) (dashboard-set-navigator t)
 :custom-face (dashboard-banner-logo-title ((t (:family "XD" :height 123))))
 :config (dashboard-setup-startup-hook)
 ;; Open Dashboard function
 (defun open-dashboard ()
   "Open the *dashboard* buffer and jump to the first widget."
   (interactive)
   (dashboard-insert-startupify-lists)
   (switch-to-buffer dashboard-buffer-name)
   (goto-char (point-min))
   (delete-other-windows)))

;; page break
(use-package
 page-break-lines
 :diminish
 :init (global-page-break-lines-mode))

;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)

;; highlight line
(global-hl-line-mode 1)

;; pretty symbols
(global-prettify-symbols-mode 1)
(defun add-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
  (setq prettify-symbols-alist
        '(("lambda" . 955)
          ("delta" . 120517)
          ("epsilon" . 120518)
          ("->" . 8594)
          ("<=" . 8804)
          (">=" . 8805))))
(add-hook 'prog-mode-hook 'add-pretty-lambda)
(add-hook 'org-mode-hook 'add-pretty-lambda)

(provide 'init-ui)
;;; init-ui.el ends here
