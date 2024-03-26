;;; lolo-org.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package org-super-agenda
  :after org
  :config
  (setq org-super-agenda-header-map nil) ;; takes over 'j'
  ;; (setq org-super-agenda-header-prefix " ◦ ") ;; There are some unicode "THIN SPACE"s after the ◦
  ;; Hide the thin width char glyph. This is dramatic but lets me not be annoyed
  (add-hook 'org-agenda-mode-hook
            #'(lambda () (setq-local nobreak-char-display nil)))
  (org-super-agenda-mode))

(use-package org-superstar
  :config
  (setq org-superstar-leading-bullet " ")
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO" . 9744)
                                          ("INPROG-TODO" . 9744)
                                          ("WORK" . 9744)
                                          ("STUDY" . 9744)
                                          ("SOMEDAY" . 9744)
                                          ("READ" . 9744)
                                          ("PROJ" . 9744)
                                          ("CONTACT" . 9744)
                                          ("DONE" . 9745)))
  ;; :hook (org-mode . org-superstar-mode)
  )

;; Removes gap when you add a new heading
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq
   ;; org-modern-star '("●" "○" "✸" "✿")
   org-modern-star '( "⌾" "✸" "◈" "◇")
   org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
   org-modern-tag nil
   org-modern-priority nil
   org-modern-todo nil
   org-modern-table nil))

(use-package org-gcal
  :defer t
  :config
  (setq org-gcal-down-days '20					;; Only fetch events 20 days into the future
        org-gcal-up-days '10					;; Only fetch events 10 days into the past
        org-gcal-recurring-events-mode 'top-level
        org-gcal-remove-api-cancelled-events t) ;; No prompt when deleting removed events
  ;; NOTE - org-gcal ids and calendar configuation is set in 'private.el' for sake of security/privacy.
  )

(use-package org-appear
  :commands (org-appear-mode)
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-hide-emphasis-markers t		;; A default setting that needs to be t for org-appear
        org-appear-autoemphasis t		;; Enable org-appear on emphasis (bold, italics, etc)
        org-appear-autolinks nil		;; Don't enable on links
        org-appear-autosubmarkers t))	;; Enable on subscript and superscript

(use-package ox-reveal
  :defer 5)

(setq org-modules '(org-habit))

(eval-after-load 'org
  '(org-load-modules-maybe t))

(use-package org-ql
  :defer t
  )

(use-package org-preview-html
  :defer t
  :config
  (setq org-preview-html-viewer 'xwidget))

(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-latex-create-formula-image-program 'dvisvgm) ;; sharper
  (plist-put org-format-latex-options :scale 1.5) ;; bigger
  (setq org-latex-preview-ltxpng-directory (concat (temporary-file-directory) "ltxpng/"))
  )

(use-package org-tree-slide
  :defer t
  :config
  (setq org-tree-slide-slide-in-effect nil
        org-tree-slide-skip-outline-level 3))

(use-package org-download
  :defer 2
  :config
  (setq org-download-method 'attach)
  (advice-add 'org-download-yank :before 'lolo/system-clipboard-to-emacs-clipboard))

(use-package valign :defer t)

(setq org-special-ctrl-a/e t)

(general-def
  :states 'normal
  :keymaps 'org-mode-map
  "t" 'org-todo
  "<return>" 'org-open-at-point-global
  "K" 'org-shiftup
  "J" 'org-shiftdown
  "<f5>" 'org-ctrl-c-ctrl-c)

(general-def
  :states 'insert
  :keymaps 'org-mode-map
  "S-<left>" 'org-shiftleft
  "S-<right>" 'org-shiftright)

(general-def
  :keymaps 'org-mode-map
  "M-[" 'org-metaleft
  "M-]" 'org-metaright
  "C-M-=" 'ap/org-count-words
  "s-r" 'org-refile
  "M-k" 'org-insert-link
  "C-c t" 'lolo/org-done-keep-todo)

;; Org-src - when editing an org source block
(general-def
  :prefix ","
  :states 'normal
  :keymaps 'org-src-mode-map
  "b" '(nil :which-key "org src")
  "bc" 'org-edit-src-abort
  "bb" 'org-edit-src-exit)

(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-mode-map)
 "" nil
 "A" '(org-archive-subtree-default :which-key "org-archive")
 "a" '(org-agenda :which-key "org agenda")
 "6" '(org-sort :which-key "sort")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-schedule :which-key "schedule")
 "S" '(lolo/org-schedule-tomorrow :which-key "schedule tmrw")
 "d" '(org-deadline :which-key "deadline")
 "g" '(counsel-org-goto :which-key "goto heading")
 "t" '(counsel-org-tag :which-key "set tags")
 "p" '(org-set-property :which-key "set property")
 "r" '(lolo/org-refile-this-file :which-key "refile in file")
 "e" '(org-export-dispatch :which-key "export org")
 "," '(lolo/org-set-startup-visibility :which-key "startup visibility")
 "." '(org-toggle-narrow-to-subtree :which-key "toggle narrow to subtree")
 "H" '(org-html-convert-region-to-html :which-key "convert region to html")
 "C" '(lolo/org-copy-link-to-clipboard :which-key "copy link to clipboard")
 "=" '(ap/org-count-words :which-key "ap/org-count-words")

 "1" '(org-toggle-link-display :which-key "toggle link display")
 "2" '(org-toggle-inline-images :which-key "toggle images")
 "3" '(lolo/org-occur-unchecked-boxes :which-key "occur unchecked boxes")

 "b" '(nil :which-key "babel")
 "bt" '(org-babel-tangle :which-key "org-babel-tangle")
 "bb" '(org-edit-special :which-key "org-edit-special")
 "bc" '(org-edit-src-abort :which-key "org-edit-src-abort")
 "bk" '(org-babel-remove-result-one-or-many :which-key "org-babel-remove-result-one-or-many")

 "x" '(nil :which-key "text")
 "xb" (spacemacs|org-emphasize spacemacs|org-bold ?*)
 "xb" (spacemacs|org-emphasize spacemacs|org-bold ?*)
 "xc" (spacemacs|org-emphasize spacemacs|org-code ?~)
 "xi" (spacemacs|org-emphasize spacemacs|org-italic ?/)
 "xs" (spacemacs|org-emphasize spacemacs|org-strike-through ?+)
 "xu" (spacemacs|org-emphasize spacemacs|org-underline ?_)
 "xv" (spacemacs|org-emphasize spacemacs|org-verbose ?~) ;; I realized that ~~ is the same and better than == (Github won't do ==)

 ;; insert
 "i" '(nil :which-key "insert")

 "il" '(org-insert-link :which-key "org-insert-link")
 "l" '(org-insert-link :which-key "org-insert-link") ;; More convenient access
 "iL" '(counsel-org-link :which-key "counsel-org-link")
 "it" '(jb-hydra-org-table/body :which-key "tables")

 "is" '(nil :which-key "insert stamp")
 "iss" '((lambda () (interactive) (call-interactively (org-time-stamp-inactive))) :which-key "org-time-stamp-inactive")
 "isS" '((lambda () (interactive) (call-interactively (org-time-stamp nil))) :which-key "org-time-stamp")

 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-clock-in :which-key "clock in")
 "co" '(org-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )


;; Org-agenda
(general-define-key
 :prefix ","
 :states 'motion
 :keymaps '(org-agenda-mode-map)
 "" nil
 "a" '(org-agenda :which-key "org agenda")
 "c" '(org-capture :which-key "org-capture")
 "s" '(org-agenda-schedule :which-key "schedule")
 "," '(org-agenda-schedule :which-key "schedule") ;; quick access
 "d" '(org-agenda-deadline :which-key "deadline")
 "t" '(org-agenda-set-tags :which-key "set tags")
 ;; clocking
 "c" '(nil :which-key "clocking")
 "ci" '(org-agenda-clock-in :which-key "clock in")
 "co" '(org-agenda-clock-out :which-key "clock out")
 "cj" '(org-clock-goto :which-key "jump to clock")
 )

(defface lolo-read
  '((t (:foreground "MediumPurple2")))
  "Custom face for highlighting read."
  :group 'lolo)

;; (defun lolo/org-highlight-setup ()
;;   (highlight-regexp "\\<\\(TODO\\|NEXT\\) \\(Read\\)\\>" 'lolo-read 2))

;; (add-hook 'org-agenda-finalize-hook 'lolo/org-highlight-setup)
;; (add-hook 'org-mode-hook 'lolo/org-highlight-setup)

(defun lolo/org-setup ()
  (org-indent-mode) ;; Keeps org items like text under headings, lists, nicely indented
  (visual-line-mode 1) ;; Nice line wrapping
;;   (centered-cursor-mode) ;; Enable centered cursor mode
  (smartparens-mode 0) ;; Disable smartparents
;;   (hl-prog-extra-mode) ;; Highlighting with regexps
  (setq-local line-spacing (+ lolo-default-line-spacing 2)) ;; A bit more line spacing for orgmode
  (valign-mode)
  )

(use-package org
  ;; :pin elpa
  :hook (org-mode . lolo/org-setup)
  :hook (org-mode . lolo/prettify-symbols-setup)
  :diminish org-indent-mode
  :diminish visual-line-mode
  :config

(setq org-ellipsis "…")
;; ⤵ ▼ ⬎  
(setq org-src-fontify-natively t) ;; Syntax highlighting in org src blocks
(setq org-highlight-latex-and-related '(native)) ;; Highlight inline LaTeX
(setq org-startup-folded 'showeverything)
(setq org-image-actual-width 300)
(setq org-fontify-whole-heading-line t)
(setq org-pretty-entities t)
(setq org-cycle-separator-lines 1)
(setq org-catch-invisible-edits 'show-and-error) ;; 'smart
(setq org-src-tab-acts-natively t)

;; M-Ret can split lines on items and tables but not headlines and not on anything else (unconfigured)
(setq org-M-RET-may-split-line '((headline) (item . t) (table . t) (default)))
(setq org-loop-over-headlines-in-active-region nil)

;; Opens links to other org file in same frame (rather than splitting)
(setq org-link-frame-setup '((file . find-file)))

(setq org-log-done t
      org-log-into-drawer t)

;; Automatically change bullet type when indenting
;; Ex: indenting a + makes the bullet a *.
(setq org-list-demote-modify-bullet
      '(("+" . "*") ("*" . "-") ("-" . "+")))

;; Automatically save and close the org files I most frequently archive to.
;; I see no need to keep them open and crowding my buffer list.
;; Uses my own function lolo/save-and-close-this-buffer.
(dolist (file '("homework-archive.org_archive" "todo-archive.org_archive"))
  (advice-add 'org-archive-subtree-default :after 
              (lambda () (lolo/save-and-close-this-buffer file))))

(defun lolo/post-org-goto ()
  (let ((current-prefix-arg '(4))) ;; emulate C-u
    (call-interactively 'org-reveal))
  (org-cycle))

(advice-add 'counsel-org-goto :after #'lolo/post-org-goto)
(advice-add 'org-agenda-goto :after #'lolo/post-org-goto)
(advice-add 'org-agenda-switch-to :after #'lolo/post-org-goto)
;; (setq org-tag-faces '(
;;                       ("Misc" . "tan1")
;;                       ("qp" . "RosyBrown1") ;; Quick-picks
;;                       ("ec" . "PaleGreen3") ;; Extracurricular
;;                       ("st" . "DimGrey") ;; Near-future (aka short term) todo
;;                       ))

(setq org-tags-column -1)
(setq org-todo-keywords '((type
                           "TODO(t)" "WAITING(h)" "INPROG-TODO(i)" "WORK(w)"
                           "STUDY(s)" "SOMEDAY" "READ(r)" "PROJ(p)" "CONTACT(c)"
                           "|" "DONE(d)" "CANCELLED(C@)")))

(setq org-todo-keyword-faces
      '(("TODO"  :inherit (region org-todo) :foreground "DarkOrange1"   :weight bold)
        ("WORK"  :inherit (org-todo region) :foreground "DarkOrange1"   :weight bold)
        ("READ"  :inherit (org-todo region) :foreground "MediumPurple2" :weight bold)
        ("PROJ"  :inherit (org-todo region) :foreground "orange3"     :weight bold)
        ("STUDY" :inherit (region org-todo) :foreground "plum3"       :weight bold)
        ("DONE" . "SeaGreen4")))

(setq org-lowest-priority ?F)  ;; Gives us priorities A through F
(setq org-default-priority ?E) ;; If an item has no priority, it is considered [#E].

(setq org-priority-faces
      '((65 . "red2")
        (66 . "Gold1")
        (67 . "Goldenrod2")
        (68 . "PaleTurquoise3")
        (69 . "DarkSlateGray4")
        (70 . "PaleTurquoise4")))

;; Org-Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (shell . t)
   (gnuplot . t)
   (java . t)
   ))

(use-package gnuplot :defer t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
(setq python-shell-completion-native-enable nil)

;; How to open buffer when calling `org-edit-special'.
(setq org-src-window-setup 'current-window)

(setq org-habit-preceding-days 6
      org-habit-following-days 6
      org-habit-show-habits-only-for-today nil
      org-habit-today-glyph ?⍟ ;;‖
      org-habit-completed-glyph ?✓
      org-habit-graph-column 40)

;; custom time stamp format. I don't use this.
(setq org-time-stamp-custom-formats '("<%A, %B %d, %Y" . "<%m/%d/%y %a %I:%M %p>"))

(setq org-agenda-restore-windows-after-quit t)

(setq org-agenda-window-setup 'current-window)

;; Only show upcoming deadlines for the next X days. By default it shows
;; 14 days into the future, which seems excessive.
(setq org-deadline-warning-days 3)
;; If something is done, don't show its deadline
(setq org-agenda-skip-deadline-if-done t)
;; If something is done, don't show when it's scheduled for
(setq org-agenda-skip-scheduled-if-done t)
;; If something is scheduled, don't tell me it is due soon
(setq org-agenda-skip-deadline-prewarning-if-scheduled t)

;; use AM-PM and not 24-hour time
(setq org-agenda-timegrid-use-ampm t)

;; A new day is 3am (I work late into the night)
;; (setq org-extend-today-until 3)

;; (setq org-agenda-time-grid '((daily today require-timed)
;;                              (1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
;;                              "        " "----------------"))

(setq org-agenda-time-grid nil)

;; (setq org-agenda-span 'day)

(setq org-agenda-block-separator ?-)
;; (setq org-agenda-current-time-string "<----------------- Now")

;; ;; (setq org-agenda-block-separator nil)

;; (setq org-agenda-scheduled-leaders '("Plan | " "Sched.%2dx: ") ; ⇛
;;       org-agenda-deadline-leaders '("Due: " "(in %1d d.) " "Due %1d d. ago: "))

;; (setq org-agenda-prefix-format '((agenda . "  %-6:T %t%s")
;;                                  (todo . "  %-6:T %t%s")
;;                                  (tags . " %i %-12:c")
;;                                  (search . " %i %-12:c")))

;;;;; more true to defaults

(setq org-agenda-prefix-format '((agenda . " %-12:T%?-12t% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-agenda-deadline-leaders '("Deadline:  " "In %2d d.: " "%2d d. ago: "))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (setq-local line-spacing 3)))

(add-hook 'org-agenda-mode-hook
          #'(lambda () (hide-mode-line-mode)))

(setq org-agenda-custom-commands nil)

(setq org-agenda-hide-tags-regexp "\\(ec\\|lit\\|sci\\|edu\\|ds\\|calc3\\)")

(defvar lolo-org-agenda-columbia-productivity-super-groups
  '((:name "Personal Items" :tag "p" :order 10)
    (:name "Extracurricular" :tag "ec" :order 5)
    (:name "Todo" :todo ("TODO") :order 3)
    (:name "Heads Up!"
           :todo ("PROJ" "WORK" "STUDY") :tag "lt" :order 4)
    (:discard (:todo t))))

(defvar lolo-org-columbia-productivity-ql-query
  '(and (not (tags "defer"))
        (not (scheduled)) ;; rationale --- if it's scheduled I don't need the heads-up
        (or (effort 1)
            (todo "TODO" "PROJ" "STUDY")
            (and (todo)
                 (tags "p" "ec" "lt")))))

;; Day View
(add-to-list 'org-agenda-custom-commands
             '("c" "Columbia Day View"
               ((agenda "" ((org-agenda-overriding-header "Columbia Productivity View")
                            (org-agenda-span 'day)
                            (org-agenda-sorting-strategy '(scheduled-up deadline-up priority-down))
                            (org-super-agenda-groups '(
                                                       (:name "Today:"
                                                              :scheduled t
                                                              :order 2)
                                                       (:name "Deadlines:"
                                                              :deadline t
                                                              :order 3)
                                                       (:name "Today's Schedule:"
                                                              :time-grid t
                                                              :discard (:deadline t)
                                                              :order 1)))))

                (org-ql-block lolo-org-columbia-productivity-ql-query
                              ((org-ql-block-header "Productivity Overview:")
                               (org-super-agenda-groups lolo-org-agenda-columbia-productivity-super-groups))))))

;; Day View No Agenda
(add-to-list 'org-agenda-custom-commands
             '("v" "Columbia Day View No Agenda"
               ((org-ql-block '(todo)
                              ((org-super-agenda-groups (push '(:name "Today's Tasks" ;; lolo-org-super-agenda-school-groups, with this added on
                                                                      :scheduled today
                                                                      :deadline today) lolo-org-agenda-columbia-productivity-super-groups)

                                                        ;; '((:name "Today's Tasks"
                                                        ;;                                  :scheduled today
                                                        ;;                                  :deadline today)
                                                        ;;                           (:discard (:tag "defer"))
                                                        ;;                           (:name "Extracurricular:"
                                                        ;;                                  :tag "ec"
                                                        ;;                                  :order 10)
                                                        ;;                           (:name "Personal:"
                                                        ;;                                  :tag "p"
                                                        ;;                                  :order 5)
                                                        ;;                           (:name "Projects"
                                                        ;;                                  :todo ("STUDY" "PROJ")
                                                        ;;                                  :tag "lt")
                                                        ;;                           (:discard (:todo t)))
                                                        ))))))

;; Three-day view
(add-to-list 'org-agenda-custom-commands
             '("w" "Columbia Four-Day View"
               ((agenda "" ((org-agenda-span 4)
                            (org-agenda-entry-types '(:deadline :scheduled))
                            (org-agenda-start-on-weekday nil)
                            (org-deadline-warning-days 0)))

                (org-ql-block lolo-org-columbia-productivity-ql-query
                              ((org-ql-block-header "Productivity Overview:")
                               (org-super-agenda-groups lolo-org-agenda-columbia-productivity-super-groups))))))

;; Six-day view
(add-to-list 'org-agenda-custom-commands
             '("q" "Columbia Ten-Day View"
               ((agenda "" ((org-agenda-span 10)
                            (org-agenda-entry-types '(:deadline :scheduled))
                            (org-agenda-start-on-weekday nil)
                            (org-deadline-warning-days 0))))))

;; By default an org-capture/refile will save a bookmark. This
;; disables that and keeps my bookmark list how I want it.
(setq org-bookmark-names-plist nil)

(setq org-export-with-broken-links t
      org-export-with-smart-quotes t
      org-export-allow-bind-keywords t)

;; From https://stackoverflow.com/questions/23297422/org-mode-timestamp-format-when-exported
(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))


;; HTML-specific
(setq org-html-validation-link nil) ;; No validation button on HTML exports

;; LaTeX Specific
(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

(use-package ox-hugo
  :defer 2
  :after ox
  :config
  (setq org-hugo-base-dir "~/code/fun/cpb"))

;; (use-package ox-moderncv
;;   :ensure nil
;;   :init (require 'ox-moderncv))

(setq org-latex-listings t) ;; Uses listings package for code exports
(setq org-latex-compiler "xelatex") ;; XeLaTex rather than pdflatex

;; not sure what this is, look into it
;; '(org-latex-active-timestamp-format "\\texttt{%s}")
;; '(org-latex-inactive-timestamp-format "\\texttt{%s}")

;; LaTeX Classes
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex" ;; I use this in base class in all of my org exports.
                 "\\documentclass{extarticle}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  )

(setq org-clock-mode-line-total 'current) ;; Show only timer from current clock session in modeline
(setq org-clock-clocked-in-display 'both)

(setq org-attach-id-dir ".org-attach/"
      org-attach-use-inheritance t)
) ;; This parenthesis ends the org use-package.

(provide 'lolo-org)
;;; lolo-org.el ends here
