;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package dashboard
  :straight (:build t)
  :ensure t
  :after all-the-icons
  :config
  (setq dashboard-banner-logo-title "Zrik Vanilla Emacs"
        dashboard-startup-banner    'logo
        dashboard-center-content    t
        dashboard-show-shortcuts    t
        dashboard-set-navigator     t
        dashboard-set-heading-icons t
        dashboard-set-file-icons    t
        initial-buffer-choice       (lambda () (get-buffer "*dashboard*"))
        dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-navigator-buttons
        `(((,(all-the-icons-faicon "level-up" :height 1.1 :v-adjust 0.0)
            "Update Packages"
            ""
            (lambda (&rest _) (progn
                                (require 'straight)
                                (straight-pull-all)
                                (straight-rebuild-all)))))))

  (setq dashboard-items '((recents  . 15)
                          (agenda   . 10)
                          (projects . 10)))
  (dashboard-setup-startup-hook)
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer))

;; ============================================================================
(use-package git-gutter-fringe
  :straight (:build t)
  :hook ((prog-mode     . git-gutter-mode)
         (org-mode      . git-gutter-mode)
         (markdown-mode . git-gutter-mode)
         (latex-mode    . git-gutter-mode)))

;; ============================================================================
(use-package all-the-icons
  :defer t
  :straight t)

(defun prog-mode-set-symbols-alist ()
  (setq prettify-symbols-alist '(("lambda"  . ?λ)))
  (prettify-symbols-mode 1))

(add-hook 'prog-mode-hook #'prog-mode-set-symbols-alist)

(setq-default lisp-prettify-symbols-alist '(("lambda"    . ?λ)
                                            ("defun"     . ?𝑓)
                                            ("defvar"    . ?𝑣)
                                            ("defcustom" . ?𝑐)
                                            ("defconst"  . ?𝐶)))

(defun lisp-mode-prettify ()
  (setq prettify-symbols-alist lisp-prettify-symbols-alist)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode 1))

(dolist (lang '(emacs-lisp lisp common-lisp scheme))
  (add-hook (intern (format "%S-mode-hook" lang))
            #'lisp-mode-prettify))

(setq prettify-symbols-unprettify-at-point t)

;; ============================================================================
(use-package ligature
  :straight (ligature :type git
                      :host github
                      :repo "mickeynp/ligature.el"
                      :build t)
  :config
  (ligature-set-ligatures 't
                          '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures '(eww-mode org-mode elfeed-show-mode)
                          '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode
                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                            ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                            "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                            "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                            "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                            "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                            "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                            "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                            ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                            "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                            "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                            "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                            "\\\\" "://"))
  (global-ligature-mode t))

;; ============================================================================
(use-package doom-modeline
  :straight (:build t)
  :defer t
  :init
  (doom-modeline-mode 1)
  (setq find-file-visit-truename t)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
  (doom-modeline-4e t)
  (doom-modeline-env-version t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  (custom-set-faces
   '(mode-line ((t (:family "IosevkaMono Nerd Font" :height 0.9))))
   '(mode-line-active
     ((t (:family "IosevkaMono Nerd Font" :height 0.9)))) ; For 29+
   '(mode-line-inactive
     ((t (:family "IosevkaMono Nerd Font" :height 0.9))))))

;; ============================================================================
(use-package valign
  :defer t
  :straight (:build t)
  :after (org markdown-mode)
  ;; :hook ((org-mode markdown-mode) . valign-mode)
  :custom ((valign-fancy-bar t)))

;; ============================================================================
(use-package secret-mode
  :defer t
  :straight (secret-mode :build t
                         :type git
                         :host github
                         :repo "bkaestner/secret-mode.el"))

;; ============================================================================
(use-package solaire-mode
  :defer t
  :straight (:build t)
  :init (solaire-global-mode +1))

;; ============================================================================
;; (when lolo-theme
;;   (require
;;    (pcase lolo-theme
;;      ('gruber-darker 'lolo-gruber-darker-theme)
;;      ('ef 'lolo-ef-theme)
;;      ('modus 'lolo-modus-theme)
;;      ('standard 'lolo-standard-theme))))
(require 'lolo-gruber-darker-theme)

;; ============================================================================
(use-package rainbow-delimiters
  :straight (:build t)
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; ============================================================================
(use-package
 page-break-lines
 :diminish
 :init (global-page-break-lines-mode))

;; ============================================================================
(use-package info-colors
  :straight (:build t)
  :commands info-colors-fnontify-node
  :hook (Info-selection . info-colors-fontify-node)
  :hook (Info-mode      . mixed-pitch-mode))

;; ============================================================================
(use-package
 winner
 :ensure nil
 :custom
 (winner-boring-buffers
  '("*Completions*"
    "*Compile-Log*"
    "*inferior-lisp*"
    "*Fuzzy Completions*"
    "*Apropos*"
    "*Help*"
    "*cvs*"
    "*Buffer List*"
    "*Ibuffer*"
    "*esh command on file*"))
 :config (winner-mode 1))

(provide 'lolo-ui)
;;; lolo-ui.el ends here
