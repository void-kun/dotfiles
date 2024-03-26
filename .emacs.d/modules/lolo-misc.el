;;; lolo-misc.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package magit :defer t)
(use-package unfill :defer t)
(use-package burly :defer t)
(use-package ace-window :defer t)
(use-package centered-cursor-mode :diminish centered-cursor-mode)
(use-package restart-emacs :defer t)
(use-package diminish)
(use-package reveal-in-osx-finder :commands (reveal-in-osx-finder))

(use-package bufler
  :config
  (setq bufler-filter-buffer-modes nil ;; Don't hide so many buffers
        bufler-filter-buffer-name-regexps nil)
  (setf bufler-groups (bufler-defgroups
                        (group
                         ;; Subgroup collecting all named workspaces.
                         (auto-workspace))
                        (group
                         ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
                         (group-or "*Help/Info*"
                                   (mode-match "*Help*" (rx bos "help-"))
                                   (mode-match "*Info*" (rx bos "info-"))))
                        (group
                         ;; Subgroup collecting all special buffers (i.e. ones that are not
                         ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
                         ;; through to other groups, so they end up grouped with their project buffers).
                         (group-and "*Special*"
                                    (lambda (buffer)
                                      (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                                           buffer)
                                                  (funcall (mode-match "Dired" (rx bos "dired"))
                                                           buffer)
                                                  (funcall (auto-file) buffer))
                                        "*Special*")))
                         (group
                          ;; Subgroup collecting these "special special" buffers
                          ;; separately for convenience.
                          (name-match "**Special**"
                                      (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
                         (group
                          ;; Subgroup collecting all other Magit buffers, grouped by directory.
                          (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
                          (auto-directory))
                         ;; Remaining special buffers are grouped automatically by mode.
                         ;; (auto-mode)
                         )
                        (group (dir "~/code/fun/notes/")
                               (auto-mode)
                               (auto-directory))
                        (group (dir "~/code/fun/org/"))
                        ;; Group remaining buffers by directory, then major mode.
                        (auto-directory)
                        (auto-mode)))
  :general
  (:keymaps 'bufler-list-mode-map "Q" 'kill-this-buffer))

(use-package hl-prog-extra
  :commands (hl-prog-extra-mode)
  :config
  (setq hl-prog-extra-list
        (list
         '("\\<\\(TODO\\|NOTE\\)\\(([^)+]+)\\)?" 0 comment
           (:weight bold :inherit diff-removed))
         ;; Match TKs in quotation marks (hl-prog-extra sees them as strings)
         '("\\(TK\\)+" 0 string '(:weight bold :inherit font-lock-warning-face))
         ;; Match TKs not in quotation marks
         '("\\(TK\\)+" 0 nil '(:weight bold :inherit font-lock-warning-face))))
  (global-hl-prog-extra-mode))

(use-package xwidget
  :general
  (general-define-key :states 'normal :keymaps 'xwidget-webkit-mode-map 
                      "j" 'xwidget-webkit-scroll-up-line
                      "k" 'xwidget-webkit-scroll-down-line
                      "gg" 'xwidget-webkit-scroll-top
                      "G" 'xwidget-webkit-scroll-bottom))

(use-package mw-thesaurus
  :defer t)

(use-package ansi-term
  :ensure nil
  :general
  (:keymaps 'term-mode-map
            "<up>" 'term-previous-input
            "<down>" 'term-next-input))

;; https://github.com/oantolin/epithet
;; (use-package epithet
;;   :ensure nil
;;   :config
;;   (add-hook 'Info-selection-hook #'epithet-rename-buffer)
;;   (add-hook 'help-mode-hook #'epithet-rename-buffer))

;; ;; https://github.com/udyantw/most-used-words
;; (use-package most-used-words :ensure nil)

(defun lolo/deft-kill ()
  (kill-buffer "*Deft*"))

(use-package deft
  :config
  (setq deft-directory (concat lolo/dropbox "notes/")
        deft-extensions '("org" "txt")
        deft-recursive t
        deft-file-limit 40
        deft-use-filename-as-title t)

  (add-hook 'deft-open-file-hook 'lolo/deft-kill) ;; Once a file is opened, kill Deft

  ;; Removes :PROPERTIES: from descriptions
  (setq deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n")
  :general

  (general-define-key :states 'normal :keymaps 'deft-mode-map
                      ;; 'q' kills Deft in normal mode
                      "q" 'kill-this-buffer)

  (general-define-key :states 'insert :keymaps 'deft-mode-map
                      "C-j" 'next-line
                      "C-k" 'previous-line)
  )

(use-package mu4e-views
  :after mu4e
  :defer t
  :config
  (setq mu4e-views-completion-method 'ivy) ;; use ivy for completion
  (setq mu4e-views-default-view-method "html") ;; make xwidgets default
  (mu4e-views-mu4e-use-view-msg-method "html") ;; select the default
  )

;; (use-package pdf-tools
;;   :defer t
;;   ;; stop pdf-tools being automatically updated when I update the
;;   ;; rest of my packages, since it would need the installation command and restart
;;   ;; each time it updated.
;;   :pin manual
;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   (pdf-loader-install)
;;   (setq-default pdf-view-display-size 'fit-height)
;;   (setq pdf-view-continuous nil) ;; Makes it so scrolling down to the bottom/top of a page doesn't switch to the next page
;;   (setq pdf-view-midnight-colors '("#ffffff" . "#121212" )) ;; I use midnight mode as dark mode, dark mode doesn't seem to work
;;   :general
;;   (general-define-key :states 'motion :keymaps 'pdf-view-mode-map
;;                       "j" 'pdf-view-next-page
;;                       "k" 'pdf-view-previous-page

;;                       "C-j" 'pdf-view-next-line-or-next-page
;;                       "C-k" 'pdf-view-previous-line-or-previous-page

;;                       ;; Arrows for movement as well
;;                       (kbd "<down>") 'pdf-view-next-line-or-next-page
;;                       (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;;                       (kbd "<down>") 'pdf-view-next-line-or-next-page
;;                       (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;;                       (kbd "<left>") 'image-backward-hscroll
;;                       (kbd "<right>") 'image-forward-hscroll

;;                       "H" 'pdf-view-fit-height-to-window
;;                       "0" 'pdf-view-fit-height-to-window
;;                       "W" 'pdf-view-fit-width-to-window
;;                       "=" 'pdf-view-enlarge
;;                       "-" 'pdf-view-shrink

;;                       "q" 'quit-window
;;                       "Q" 'kill-this-buffer
;;                       "g" 'revert-buffer

;;                       "C-s" 'isearch-forward
;;                       )
;;   )

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1))

(provide 'lolo-misc)
;;; lolo-misc.el ends here
