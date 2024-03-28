;;; lolo-completion.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package company
  :diminish company-mode
  :general
  (general-define-key :keymaps 'company-active-map
                      "C-c C-j" 'company-select-next
                      "C-c C-k" 'company-select-previous)
  :init
  ;; These configurations come from Doom Emacs:
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-require-match 'never
        company-global-modes '(not erc-mode message-mode help-mode gud-mode)
        company-frontends
        '(company-pseudo-tooltip-frontend  ; always show candidates in overlay tooltip
          company-echo-metadata-frontend)  ; show selected candidate docs in echo area
        company-backends '(company-capf company-files company-keywords)
        company-auto-complete nil
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)

  :config
  (setq company-idle-delay 0.3)
  :custom-face
  (company-tooltip ((t (:family "RobotoMono Nerd Font")))))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  )


(use-package kind-icon
  :config
  ;; (setq kind-icon-default-face 'corfu-default)
  (setq kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1))
  (setq kind-icon-blend-frac 0.08)
  ;; (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'counsel-load-theme #'(lambda () (interactive) (kind-icon-reset-cache)))
  (add-hook 'load-theme         #'(lambda () (interactive) (kind-icon-reset-cache))))

(use-package ivy
  :diminish ivy-mode
  :config
  (setq ivy-extra-directories nil) ;; Hides . and .. directories
  (setq ivy-initial-inputs-alist nil) ;; Removes the ^ in ivy searches
  (setq-default ivy-height 11)
  (setq ivy-fixed-height-minibuffer t)
  (add-to-list 'ivy-height-alist '(counsel-M-x . 7)) ;; Don't need so many lines for M-x, I usually know what command I want

  (ivy-mode 1)

  ;; Shows a preview of the face in counsel-describe-face
  (add-to-list 'ivy-format-functions-alist '(counsel-describe-face . counsel--faces-format-function)))

;; Nice icons in Ivy. Replaces all-the-icons-ivy.
(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-icon-size 1.0))

(use-package ivy-rich
  :after ivy
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :config
  (setq default-directory lolo/home)
  (setq counsel-switch-buffer-preview-virtual-buffers nil) ;; Removes recentfiles/bookmarks from counsel-switch-buffer
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; That weird Icon? file in Dropbox.
         "\\(Icon\
\\)"
         ;; Hides file names beginning with # or .
         "\\|\\(?:\\`[#.]\\)"))

  ;; emacs regexp notes: had to put \\| before the second regexp to make this work

  ;; Sorts counsel-recentf in order of time last accessed
  (add-to-list 'ivy-sort-functions-alist
               '(counsel-recentf . file-newer-than-file-p))

  (add-to-list 'recentf-exclude
               (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

  ;; Use fd
  (setq find-program "fd")
  (setq counsel-file-jump-args (split-string "-L --type f -H")) ;; follow symlinks, files, show hidden

  :general
  (general-define-key :keymaps 'counsel-find-file-map
                      "C-c f" 'counsel-file-jump-from-find)
  )

(use-package prescient
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000) ;; More prescient history
  (prescient-persist-mode +1))

;; Use `prescient' for Ivy menus.
(use-package ivy-prescient
  :after ivy
  :config
  ;; don't prescient sort these commands
  (dolist (command '(org-ql-view counsel-find-file fontaine-set-preset))
    (setq ivy-prescient-sort-commands (append ivy-prescient-sort-commands (list command))))
  (ivy-prescient-mode +1))

;; (use-package company-prescient
;;   :defer 2
;;   :after company
;;   :config
;;   (company-prescient-mode +1))

(use-package smartparens
  :diminish smartparens-mode
  :defer 1
  :config
  ;; Load default smartparens rules for various languages
  (require 'smartparens-config)
  (setq sp-max-prefix-length 25)
  (setq sp-max-pair-length 4)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))

  ;; In lisps ( should open a new form if before another parenthesis
  (sp-local-pair sp-lisp-modes "(" ")" :unless '(:rem sp-point-before-same-p))

  ;; Don't do square-bracket space-expansion where it doesn't make sense to
  (sp-local-pair '(emacs-lisp-mode org-mode markdown-mode gfm-mode)
                 "[" nil :post-handlers '(:rem ("| " "SPC")))


  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil
             :post-handlers '(("||\n[i]" "RET") ("| " "SPC"))
             ;; Don't autopair opening braces if before a word character or
             ;; other opening brace. The rationale: it interferes with manual
             ;; balancing of braces, and is odd form to have s-exps with no
             ;; whitespace in between, e.g. ()()(). Insert whitespace if
             ;; genuinely want to start a new form in the middle of a word.
             :unless '(sp-point-before-word-p sp-point-before-same-p)))
  (smartparens-global-mode t))

;; "Enable Flyspell mode, which highlights all misspelled words. "
(use-package flyspell
  :defer t
  :config
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_EXPORT" . "^#\\+END_EXPORT"))
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))

  (dolist (mode '(rust-mode-hook
				  go-mode-hook
				  c++-mode-hook
				  c-mode-hook
				  html-mode-hook
				  css-mode-hook
				  js-mode-hook))
    (add-hook mode (lambda () (flyspell-prog-mode 1))))

  
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  )

(use-package avy
  :defer t
  :config
  (setq avy-case-fold-search nil))

(use-package simpleclip :config (simpleclip-mode 1))

;; Allows pasting in minibuffer with M-v
(defun lolo/paste-in-minibuffer ()
  (local-set-key (kbd "M-v") 'simpleclip-paste))
(add-hook 'minibuffer-setup-hook 'lolo/paste-in-minibuffer)

(use-package undo-fu)

(use-package yasnippet
  :diminish yas-minor-mode
  :defer 5
  :config
  (setq yas-snippet-dirs (list (expand-file-name "snippets" lolo/emacs-stuff)))
  (yas-global-mode 1)) ;; or M-x yas-reload-all if you've started YASnippet already.

;; Silences the warning when running a snippet with backticks (runs a command in the snippet)
(require 'warnings)
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))


(provide 'lolo-completion)
;;; lolo-completion.el ends here
