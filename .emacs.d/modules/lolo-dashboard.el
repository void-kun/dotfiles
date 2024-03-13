
;;; lolo-dashboard.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(eval-when-compile
  (require 'lolo-custom))

;; Dashboard
(use-package dashboard
  :diminish dashboard-mode
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-items-face ((t (:weight normal))))
  (dashboard-no-items-face ((t (:weight normal))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-view_dashboard")
    :color pink :quit-key ("q" "C-g"))
    ("Navigator"
    (("U" update-config-and-packages "update" :exit t)
      ("H" browse-homepage "homepage" :exit t)
      ("R" restore-session "recover session" :exit t)
      ("S" find-custom-file "settings" :exit t))
    "Section"
    (("}" dashboard-next-section "next")
      ("{" dashboard-previous-section "previous")
      ("r" dashboard-goto-recent-files "recent files")
      ("m" dashboard-goto-bookmarks "bookmarks")
      ("p" dashboard-goto-projects "projects"))
    "Item"
    (("RET" widget-button-press "open" :exit t)
      ("<tab>" widget-forward "next")
      ("C-i" widget-forward "next")
      ("<backtab>" widget-backward "previous")
      ("C-n" next-line "next line")
      ("C-p" previous-line "previous  line"))
    "Misc"
    (("<f2>" open-dashboard "open" :exit t)
      ("g" dashboard-refresh-buffer "refresh" :exit t)
      ("Q" quit-dashboard "quit" :exit t))))
  :bind (("<f2>" . open-dashboard)
          :map dashboard-mode-map
          ("H" . browse-homepage)
          ("R" . restore-session)
          ("S" . find-custom-file)
          ("U" . update-config-and-packages)
          ("q" . quit-dashboard)
          ("h" . dashboard-hydra/body)
          ("?" . dashboard-hydra/body))
  :hook (dashboard-mode . (lambda ()
                            ;; No title
                            (setq-local frame-title-format nil)
                            ;; Enable `page-break-lines-mode'
                            (when (fboundp 'page-break-lines-mode)
                              (page-break-lines-mode 1))))
  :init
  (setq dashboard-banner-logo-title "LOLO EMACS - Enjoy Programming & Writing"
        dashboard-page-separator "\n\f\n"
        dashboard-projects-backend 'project-el
        dashboard-path-style 'truncate-middle
        dashboard-path-max-length 60
        dashboard-center-content t
        dashboard-show-shortcuts nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))

        dashboard-display-icons-p #'icons-displayable-p
        dashboard-set-file-icons lolo-icon
        dashboard-set-heading-icons lolo-icon
        dashboard-heading-icons '((recents   . "nf-oct-history")
                                  (bookmarks . "nf-oct-bookmark")
                                  (agenda    . "nf-oct-calendar")
                                  (projects  . "nf-oct-briefcase")
                                  (registers . "nf-oct-database"))

        dashboard-set-navigator t
        dashboard-navigator-buttons
        `(((,(when (icons-displayable-p)
                (nerd-icons-mdicon "nf-md-github" :height 1.4))
            "Homepage" "Browse homepage"
            (lambda (&rest _) (browse-url lolo-homepage)))
            (,(when (icons-displayable-p)
                (nerd-icons-mdicon "nf-md-backup_restore" :height 1.5))
            "Restore" "Restore previous session"
            (lambda (&rest _) (restore-session)))
            (,(when (icons-displayable-p)
                (nerd-icons-mdicon "nf-md-tools" :height 1.3))
            "Settings" "Open custom file"
            (lambda (&rest _) (find-file custom-file)))
            (,(if (icons-displayable-p)
                  (nerd-icons-mdicon "nf-md-help" :height 1.2)
                "?")
            "" "Help (?/h)"
            (lambda (&rest _) (dashboard-hydra/body)))))

        dashboard-set-footer t
        dashboard-footer-icon
        (if (icons-displayable-p)
            (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)
          (propertize ">" 'face 'dashboard-footer)))

  (dashboard-setup-startup-hook)
  :config
  ;; Insert copyright
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/219
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (when dashboard-set-footer
      (dashboard-insert-center
        (propertize (format "\nPowered by Vincent Zhang, %s\n" (format-time-string "%Y"))
                    'face 'font-lock-comment-face))))
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

  (defun restore-session ()
    "Restore the previous session."
    (interactive)
    (message "Restoring previous session...")
    (quit-window t)
    (cond
      ((bound-and-true-p tabspaces-mode)
      (tabspaces-restore-session))
      ((bound-and-true-p desktop-save-mode)
      (desktop-read)))
    (message "Restoring previous session...done"))

  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))

  (defun dashboard-goto-projects ()
    "Go to projects."
    (interactive)
    (let ((func (local-key-binding "p")))
      (and func (funcall func))))

  (defun dashboard-goto-bookmarks ()
    "Go to bookmarks."
    (interactive)
    (let ((func (local-key-binding "m")))
      (and func (funcall func))))

  (defvar dashboard-recover-layout-p nil
    "Wether recovers the layout.")

  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    ;; Check if need to recover layout
    (if (length> (window-list-1)
                  ;; exclude `treemacs' window
                  (if (and (fboundp 'treemacs-current-visibility)
                          (eq (treemacs-current-visibility) 'visible))
                      2
                    1))
        (setq dashboard-recover-layout-p t))

    ;; Display dashboard in maximized window
    (delete-other-windows)

    ;; Refresh dashboard buffer
    (dashboard-refresh-buffer)

    ;; Jump to the first section
    (dashboard-goto-recent-files))

  (defun quit-dashboard ()
    "Quit dashboard window."
    (interactive)
    (quit-window t)
    (and dashboard-recover-layout-p
          (and (bound-and-true-p winner-mode) (winner-undo))
          (setq dashboard-recover-layout-p nil))))
        
(provide 'lolo-dashboard)
;;; lolo-dashboard.el ends here