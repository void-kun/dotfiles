;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package transient :straight (:build t) :defer t)

;; ============================================================================
(use-package calendar :straight (:type built-in) :defer t)

;; ============================================================================
(use-package
 dockerfile-mode
 :defer t
 :straight (:build t)
 :hook (dockerfile-mode . lsp-deferred)
 :init (put 'docker-image-name 'safe-local-variable #'stringp)
 :mode "Dockerfile\\'")

(use-package docker :defer t :straight (:build t))

;; ============================================================================
(use-package
 elfeed
 :defer t
 :straight (:build t)
 :custom
 ((elfeed-search-filter "@6-months-ago")
  (elfeed-db-directory (expand-file-name ".elfeed-db" lolo-dir))))

(defun lolo/elfeed-filter-youtube-videos (orig-fun &rest args)
  "Open with mpv the video leading to PATH"
  (let ((link (elfeed-entry-link elfeed-show-entry)))
    (when link
      (if (string-match-p ".*youtube\.com.*watch.*" link)
          (progn
            (require 'ytplay)
            (ytplay link))
        (apply orig-fun args)))))

(advice-add 'elfeed-show-visit :around #'lolo/elfeed-filter-youtube-videos)

;; ============================================================================
(use-package
 magit
 :straight (:build t)
 :defer t
 :init (setq forge-add-default-bindings nil)
 :config
 (add-hook
  'magit-process-find-password-functions 'magit-process-password-auth-source)
 (csetq
  magit-clone-default-directory
  "~/workspace/dev/source/"
  magit-display-buffer-function
  #'magit-display-buffer-same-window-except-diff-v1))

(defun my--tramp-send-command--workaround-stty-icanon-bug
    (conn-vec orig-command &rest args)
  "See: https://github.com/magit/magit/issues/4720"
  (let ((command
         (if (string= "stty -icrnl -icanon min 1 time 0" orig-command)
             "stty -icrnl"
           orig-command)))
    (append (list conn-vec command) args)))

(defun my--tramp-send-command--workaround-stty-icanon-bug--filter-args (args)
  (apply #'my--tramp-send-command--workaround-stty-icanon-bug args))

(advice-add
 'tramp-send-command
 :filter-args #'my--tramp-send-command--workaround-stty-icanon-bug--filter-args)

(defun my/magit-log-highlight-angular-keywords (_rev msg)
  "Highlight angular-style keywords in commit messages."
  (let ((boundary 0))
    (when (string-match (rx
                         (seq
                          (or "feat"
                              "fix"
                              "docs"
                              "style"
                              "refactor"
                              "perf"
                              "test"
                              "chore")
                          (* "(" (* (not ")")) ")") ":"))
                        msg
                        boundary)
      (setq boundary (match-end 0))
      (magit--put-face (match-beginning 0) boundary 'magit-keyword msg)))
  msg)

(advice-add
 #'magit-log-propertize-keywords
 :after #'my/magit-log-highlight-angular-keywords)

(use-package
 hl-todo
 :defer t
 :straight (:build t)
 :init (global-hl-todo-mode 1))

(use-package
 magit-todos
 :straight (:build t)
 :after (magit hl-todo)
 :init
 (with-eval-after-load 'magit
   (defun my/magit-todos-if-not-yadm ()
     "Deactivate magit-todos if in yadm Tramp connection.
If `magit--default-directory' points to a yadm Tramp directory,
deactivate `magit-todos-mode', otherwise enable it."
     (if (string-prefix-p "/yadm:" magit--default-directory)
         (magit-todos-mode -1)
       (magit-todos-mode +1)))
   (add-hook 'magit-mode-hook #'my/magit-todos-if-not-yadm))
 :config (setq magit-todos-ignore-case nil))

;; ============================================================================
;; Access Git forges from Magit
(use-package
forge
:demand t
:straight (:build t)
:custom-face
(forge-topic-label
  ((t
    (:inherit
    variable-pitch
    :height 0.9
    :width condensed
    :weight regular
    :underline nil))))
:init
(setq forge-topic-list-columns
      '(("#"
          5
          forge-topic-list-sort-by-number
          (:right-align t)
          number
          nil)
        ("Title" 60 t nil title nil)
        ("State" 6 t nil state nil)
        ("Updated" 10 t nil updated nil))))

  (use-package
   transient-posframe
   :diminish
   :straight (:build t)
   :defines posframe-border-width
   :custom-face (transient-posframe ((t (:inherit tooltip))))
   (transient-posframe-border
    ((t (:inherit posframe-border :background unspecified))))
   :hook (after-init . transient-posframe-mode)
   :init
   (setq
    transient-posframe-border-width posframe-border-width
    transient-posframe-min-height nil
    transient-posframe-min-width 80
    transient-posframe-poshandler 'posframe-poshandler-frame-center
    transient-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
   :config
   (with-no-warnings
     (defun my-transient-posframe--show-buffer (buffer _alist)
       "Show BUFFER in posframe and we do not use _ALIST at this period."
       (when (posframe-workable-p)
         (let*
             ((posframe
               (posframe-show
                buffer
                :height
                (with-current-buffer buffer
                  (1- (count-screen-lines (point-min) (point-max))))
                :font transient-posframe-font
                :position (point)
                :poshandler transient-posframe-poshandler
                :background-color
                (face-attribute 'transient-posframe :background nil t)
                :foreground-color
                (face-attribute 'transient-posframe :foreground nil t)
                :min-width transient-posframe-min-width
                :min-height transient-posframe-min-height
                :internal-border-width transient-posframe-border-width
                :internal-border-color
                (face-attribute 'transient-posframe-border :background
                                nil t)
                :override-parameters transient-posframe-parameters)))
           (frame-selected-window posframe))))
     (advice-add
      #'transient-posframe--show-buffer
      :override #'my-transient-posframe--show-buffer)

     (defun my-transient-posframe--hide ()
       "Hide transient posframe."
       (posframe-hide transient--buffer-name))
     (advice-add
      #'transient-posframe--delete
      :override #'my-transient-posframe--hide)))

;; Walk through git revisions of a file
(use-package
 git-timemachine
 :straight (:build t)
 :custom-face
 (git-timemachine-minibuffer-author-face
  ((t (:inherit success :foreground unspecified))))
 (git-timemachine-minibuffer-detail-face
  ((t (:inherit warning :foreground unspecified))))
 :bind (:map vc-prefix-map ("t" . git-timemachine))
 :hook
 ((git-timemachine-mode
   .
   (lambda ()
     "Improve `git-timemachine' buffers."
     ;; Display different colors in mode-line
     (if (facep 'mode-line-active)
         (face-remap-add-relative 'mode-line-active 'custom-state)
       (face-remap-add-relative 'mode-line 'custom-state))

     ;; Highlight symbols in elisp
     (and (derived-mode-p 'emacs-lisp-mode)
          (fboundp 'highlight-defined-mode)
          (highlight-defined-mode t))

     ;; Display line numbers
     (and (derived-mode-p 'prog-mode 'yaml-mode)
          (fboundp 'display-line-numbers-mode)
          (display-line-numbers-mode t))))
  (before-revert
   .
   (lambda ()
     (when (bound-and-true-p git-timemachine-mode)
       (user-error "Cannot revert the timemachine buffer"))))))

;; Pop up last commit information of current line
(use-package
 git-messenger
 :straight (:build t)
 :bind
 (:map
  vc-prefix-map ("p" . git-messenger:popup-message)
  :map git-messenger-map ("m" . git-messenger:copy-message))
 :init
 (setq
  git-messenger:show-detail t
  git-messenger:use-magit-popup t)
 :config
 (with-no-warnings
   (with-eval-after-load 'hydra
     (defhydra
      git-messenger-hydra
      (:color blue)
      ("s" git-messenger:popup-show "show")
      ("c" git-messenger:copy-commit-id "copy hash")
      ("m" git-messenger:copy-message "copy message")
      ("," (catch 'git-messenger-loop
         (git-messenger:show-parent))
       "go parent")
      ("q" git-messenger:popup-close "quit")))

   (defun my-git-messenger:format-detail
       (vcs commit-id author message)
     (if (eq vcs 'git)
         (let ((date (git-messenger:commit-date commit-id))
               (colon (propertize ":" 'face 'font-lock-comment-face)))
           (concat
            (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                    (propertize "Commit"
                                'face
                                'font-lock-keyword-face)
                    colon
                    (propertize (substring commit-id 0 8)
                                'face
                                'font-lock-comment-face)
                    (propertize "Author"
                                'face
                                'font-lock-keyword-face)
                    colon
                    (propertize author 'face 'font-lock-string-face)
                    (propertize "Date" 'face 'font-lock-keyword-face)
                    colon
                    (propertize date 'face 'font-lock-string-face))
            (propertize (make-string 38 ?─)
                        'face
                        'font-lock-comment-face)
            message
            (propertize "\nPress q to quit"
                        'face
                        '(:inherit (font-lock-comment-face italic)))))
       (git-messenger:format-detail vcs commit-id author message)))

   (defun my-git-messenger:popup-message ()
     "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
     (interactive)
     (let* ((hydra-hint-display-type 'message)
            (vcs (git-messenger:find-vcs))
            (file (buffer-file-name (buffer-base-buffer)))
            (line (line-number-at-pos))
            (commit-info
             (git-messenger:commit-info-at-line vcs file line))
            (commit-id (car commit-info))
            (author (cdr commit-info))
            (msg (git-messenger:commit-message vcs commit-id))
            (popuped-message
             (if (git-messenger:show-detail-p commit-id)
                 (my-git-messenger:format-detail
                  vcs commit-id author msg)
               (cl-case
                vcs (git msg)
                (svn
                 (if (string= commit-id "-")
                     msg
                   (git-messenger:svn-message msg)))
                (hg msg)))))
       (setq
        git-messenger:vcs vcs
        git-messenger:last-message msg
        git-messenger:last-commit-id commit-id)
       (run-hook-with-args 'git-messenger:before-popup-hook
                           popuped-message)
       (git-messenger-hydra/body)
       (cond
        ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
         (let ((buffer-name "*git-messenger*"))
           (posframe-show
            buffer-name
            :string
            (concat
             (propertize "\n" 'face '(:height 0.3))
             popuped-message
             "\n"
             (propertize "\n" 'face '(:height 0.3)))
            :left-fringe 8
            :right-fringe 8
            :max-width (round (* (frame-width) 0.62))
            :max-height (round (* (frame-height) 0.62))
            :internal-border-width 1
            :internal-border-color (face-background 'posframe-border nil t)
            :background-color (face-background 'tooltip nil t))
           (unwind-protect
               (push (read-event) unread-command-events)
             (posframe-hide buffer-name))))
        ((and (fboundp 'pos-tip-show) (display-graphic-p))
         (pos-tip-show popuped-message))
        ((fboundp 'lv-message)
         (lv-message popuped-message)
         (unwind-protect
             (push (read-event) unread-command-events)
           (lv-delete-window)))
        (t
         (message "%s" popuped-message)))
       (run-hook-with-args 'git-messenger:after-popup-hook
                           popuped-message)))
   (advice-add #'git-messenger:popup-close :override #'ignore)
   (advice-add
    #'git-messenger:popup-message
    :override #'my-git-messenger:popup-message)))

;; ============================================================================
(use-package ripgrep :if (executable-find "rg") :straight (:build t) :defer t)

(use-package
 projectile
 :straight (:build t)
 :diminish projectile-mode
 :init (setq projectile-switch-project-action #'projectile-dired)
 :config
 (projectile-mode 1)
 (add-to-list 'projectile-globally-ignored-directories "node_modules")
 (setq projectile-track-known-projects-automatically nil))

(use-package
 counsel-projectile
 :straight (:build t)
 :after (counsel projectile)
 :config (counsel-projectile-mode))

;; ============================================================================
(use-package
 screenshot
 :defer t
 :straight
 (screenshot :build t :type git :host github :repo "tecosaur/screenshot")
 :config (load-file (locate-library "screenshot.el")))

;; ============================================================================
(use-package
 wttrin
 :defer t
 :straight (wttrin :build t :host github :repo "bcbcarl/emacs-wttrin" :type git)
 :config
 (setq
  wttrin-default-cities '("Ho Chi Minh City")
  wttrin-use-metric t))

(provide 'lolo-applications)
;;; lolo-applications.el ends here
