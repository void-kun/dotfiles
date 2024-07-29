;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 avy
 :defer t
 :straight t
 :config
 (csetq
  avy-keys '(?a ?u ?i ?e ?c ?t ?s ?r ?n) avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?T . avy-action-teleport)
    (?m . avy-action-mark)
    (?C . avy-action-copy)
    (?y . avy-action-yank)
    (?Y . avy-action-yank-line)
    (?I . avy-action-ispell)
    (?z . avy-action-zap-to-char)))
 (defun my/avy-goto-url ()
   "Jump to url with avy."
   (interactive)
   (avy-jump "https?://"))
 (defun my/avy-open-url ()
   "Open url selected with avy."
   (interactive)
   (my/avy-goto-url)
   (browse-url-at-point)))


(setq
 calc-angle-mode 'rad
 calc-symbolic-mode t)

(use-package
 elcord
 :straight (:built t)
 :defer t
 :config
 (csetq
  elcord-use-major-mode-as-main-icon
  t
  elcord-refresh-rate
  5
  elcord-boring-buffers-regexp-list
  `("^ " ,(rx "*" (+ any) "*") ,(rx bol (or "Re: " "Fwd: ")))))

(use-package
 elpher
 :straight (:build t)
 :defer t
 :custom (elpher-default-url-type "gemini"))

(use-package
 keycast
 :defer t
 :straight (:build t)
 :config
 (define-minor-mode keycast-mode
   "Show current command and its key binding in the mode line."
   :global
   t
   (if keycast-mode
       (add-hook 'pre-command-hook 'keycast--update t)
     (remove-hook 'pre-command-hook 'keycast--update)))
 (add-to-list 'global-mode-string '("" mode-line-keycast " ")))

(use-package
 keyfreq
 :straight (:build t)
 :init
 (keyfreq-mode 1)
 (keyfreq-autosave-mode 1)
 :custom
 (keyfreq-file (expand-file-name "keyfreq.el" user-emacs-directory))
 (keyfreq-file-lock (expand-file-name "keyfreq.lock" user-emacs-directory))
 (keyfreq-excluded-commands
  '(self-insert-command org-self-insert-command
                        ivy-next-line
                        text-scale-pinch
                        lsp-ui-doc--handle-mouse-movement
                        vterm--self-insert
                        mouse-set-point
                        mouse-drag-region
                        mwheel-scroll
                        pdf-util-image-map-mouse-event-proxy
                        mouse-set-region)))

(use-package
 mastodon
 :defer t
 :ensure t
 :straight (mastodon :type git :host codeberg :repo "martianh/mastodon.el")
 :config
 (setq
  mastodon-instance-url "https://emacs.ch"
  mastodon-active-user "phundrak")

 (defun me/mastodon-toot--send-language-if-none ()
   (unless mastodon-toot--language
     (mastodon-toot--set-toot-language)))
 (advice-add
  #'me/mastodon-toot--send-language-if-none
  :before #'mastodon-toot--send))

(use-package
 mediawiki
 :defer t
 :straight (:build t)
 :custom
 (mediawiki-site-alist
  '(("PhundrakWiki" ; Title
     "https://wiki.phundrak.com/" ; URL
     "phundrak" ; username
     nil ; password
     nil ; LDAP
     "Main Page")))) ; Default page

(use-package
 password-gen
 :straight
 (password-gen
  :build t
  :type git
  :repo "https://labs.phundrak.com/phundrak/password-gen.el.git")
 :defer t)

(use-package pinentry :straight (:build t) :defer nil :init (pinentry-start))

(use-package sicp :straight (:build t) :defer t)

(use-package
 quick-find-files
 :defer t
 :straight
 (quick-find-files
  :type git
  :host github
  :repo "phundrak/quick-find-files.el"
  :build t)
 :custom (quick-find-files-program 'fd)
 (quick-find-files-dirs
  '((:dir "~/org" :ext "org" :ignored ("config"))
    (:dir "~/org/notes" :ext "md")))
 (quick-find-files-fd-additional-options "-L"))

(use-package winum :straight (:build t) :init (winum-mode))

(use-package
 ytplay
 :defer t
 :straight
 (ytplay
  :build t
  :type git
  :repo "https://labs.phundrak.com/phundrak/ytplay.el"))

(provide 'lolo-misc)
;;; lolo-misc.el ends here
