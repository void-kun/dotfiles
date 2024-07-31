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
       '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
         ("Title" 60 t nil title nil)
         ("State" 6 t nil state nil)
         ("Updated" 10 t nil updated nil))))

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
