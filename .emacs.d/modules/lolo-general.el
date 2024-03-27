;;; lolo-general.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package general)

(global-set-key (kbd "<escape>")      'keyboard-escape-quit)
(general-define-key
 :keymaps 'override

 ;; Top level functions
;;  "C-/"      '(lolo/rg :which-key "ripgrep")
 "C-:"      '(project-find-file :which-key "p-find file")
 "C-."      '(counsel-find-file :which-key "find file")
 "C-,"      '(counsel-recentf :which-key "recent files")
 "C-<tab>"  '(switch-to-prev-buffer :which-key "previous buffer")
 "C-q"      '(save-buffers-kill-terminal :which-key "quit emacs")
 "C-r"      '(jump-to-register :which-key "registers")

 "M-s-<left>"  '(move-beginning-of-line :which-key "move beginning of line")
 "M-s-<right>" '(move-end-of-line :which-key "move end of line")
 "M-s-<up>" '(lolo/move-text-up :which-key "move text up")
 "M-s-<down>" '(lolo/move-text-down :which-key "move text down")

 "M-s-<return>" '(lolo/newline-with-indent-below :which-key "newline with indent below")
 "C-M-s-<return>" '(lolo/newline-with-indent-above :which-key "newline with indent above")

 "C-S-c C-S-c" '(mc/edit-lines :which-key "edit lines")
 "C->" '(mc/mark-next-like-this :which-key "mark next like this")
 "C-<" '(mc/mark-previous-like-this :which-key "mark previous like this")
 "C-c C-<" '(mc/mark-all-like-this :which-key "mark all like this")

;;  ;; "Applications"
 "C-a" '(nil :which-key "applications")
 "C-a C-c" '(calc :which-key "calc")
 "C-a C-d" '(dired :which-key "dired")
 "C-a C-f" '(browse-url-firefox :which-key "firefox")
 "C-a C-x" '(xwidget-webkit-browse-url :which-key "xwidget")

 ;; Buffers
 "C-b" '(nil :which-key "buffer")
 "C-b C-b" '(counsel-switch-buffer :which-key "switch buffers")
 "C-b C-s" '(lolo/switch-to-scratch-buffer :which-key "scratch buffer")
 "C-b C-m" '(lolo/kill-other-buffers :which-key "kill other buffers")
 "C-b C-i" '(clone-indirect-buffer  :which-key "indirect buffer")
 "C-b C-r" '(revert-buffer :which-key "revert buffer")
 "C-b C-w" '(kill-this-buffer :which-key "kill this buffer")

 ;; Files
 "C-f" '(nil :which-key "files")
 "C-f C-b" '(counsel-bookmark :which-key "bookmarks")
 "C-f C-f" '(counsel-find-file :which-key "find file")
 "C-f C-n" '(spacemacs/new-empty-buffer :which-key "new file")
 "C-f C-r" '(counsel-recentf :which-key "recent files")
 "C-f C-R" '(rename-file :which-key "rename file")
 "C-f C-s" '(save-buffer :which-key "save buffer")
 "C-f C-o" '(reveal-in-osx-finder :which-key "reveal in finder")

;; Jake
"C-j" '(nil :which-key "jake")
"C-j C-b" '((lambda() (interactive) (find-file (concat lolo/dropbox "org/work.org"))) :which-key "work.org")
"C-j C-c" '((lambda() (interactive) (find-file (concat lolo/dropbox "org/cpb.org"))) :which-key "cpb.org")
"C-j C-r" '(restart-emacs :which-key "restart emacs")

"C-j C-h" '(nil :which-key "hydras")
"C-j C-h C-t" '(jb-hydra-theme-switcher/body :which-key "themes")
"C-j C-h C-f" '(jb-hydra-variable-fonts/body :which-key "mixed-pitch face")
"C-j C-h C-w" '(jb-hydra-window/body :which-key "window control")

"C-j C-m" '(nil :which-key "macros/custom commands")
"C-j C-m C-l" '(lolo/listify :which-key "Listify")
"C-j C-m C-L" '(lolo|SubListify :which-key "SubListify")
"C-j C-m C-o" '(lolo/org-temp-export-html :which-key "org temp export region")
"C-j C-k" '(nil :which-key "agenda/ql")
"C-j C-k C-q" '((lambda () (interactive) (org-ql-view "Jake Work Full View")) :which-key "jake ql")

 ;; Help/emacs
 "C-h" '(nil :which-key "help/emacs")
 "C-h C-v" '(counsel-describe-variable :which-key "des. variable")
 "C-h C-b" '(counsel-descbinds :which-key "des. bindings")
 "C-h C-M" '(describe-mode :which-key "des. mode")
 "C-h C-f" '(counsel-describe-function :which-key "des. func")
 "C-h C-F" '(counsel-describe-face :which-key "des. face")
 "C-h C-k" '(describe-key :which-key "des. key")
 "C-h C-e C-d" '((lambda () (interactive) (jump-to-register 67)) :which-key "edit dotfile")
 
 "C-h C-m" '(nil :which-key "switch mode")
 "C-h C-m C-e" '(emacs-lisp-mode :which-key "elisp mode")
 "C-h C-m C-o" '(org-mode :which-key "org mode")
 "C-h C-m C-t" '(text-mode :which-key "text mode")
 
 "C-h C-p" '(nil :which-key "packages")
 "C-h C-p C-r" 'package-refresh-contents
 "C-h C-p C-i" 'package-install
 "C-h C-p C-d" 'package-delete

 ;; Help/emacs
 "C-x" '(nil :which-key "text")
 "C-x C-C" '(lolo/copy-whole-buffer-to-clipboard :which-key "copy whole buffer to clipboard")
 "C-x C-r" '(anzu-query-replace :which-key "find and replace")
 "C-x C-s" '(yas-insert-snippet :which-key "insert yasnippet")
 "C-x C-f" '(flush-lines :which-key "flush-lines")
 "C-x C-R" '(replace-regexp :which-key "replace-regexp")

 ;; Toggles
 "C-t" '(nil :which-key "toggles")
 "C-t C-t" '(toggle-truncate-lines :which-key "truncate lines")
 "C-t C-v" '(visual-line-mode :which-key "visual line mode")
 "C-t C-n" '(display-line-numbers-mode :which-key "display line numbers")
 "C-t C-a" '(mixed-pitch-mode :which-key "variable pitch mode")
 "C-t C-y" '(counsel-load-theme :which-key "load theme")
 "C-t C-w" '(writeroom-mode :which-key "writeroom-mode")
 "C-t C-R" '(read-only-mode :which-key "read only mode")
 "C-t C-I" '(toggle-input-method :which-key "toggle input method")
 "C-t C-r" '(display-fill-column-indicator-mode :which-key "fill column indicator")
 "C-t C-m" '(hide-mode-line-mode :which-key "hide modeline mode")

 ;; Windows
 "M-<tab>" '(next-window-any-frame :which-key "next window any frame")
 "C-w" '(nil :which-key "window")
 "C-w C-m" '(lolo/toggle-maximize-buffer :which-key "maximize buffer")
 "C-w C-N" '(make-frame :which-key "make frame")
 "C-w C--" '(lolo/split-window-vertically-and-switch :which-key "split below")
 "C-w C-/" '(lolo/split-window-horizontally-and-switch :which-key "split right")
 "C-w C-r" '(jb-hydra-window/body :which-key "hydra window")
 "C-w C-z" '(text-scale-adjust :which-key "text zoom")
 
 "C-M-s-<up>" '(enlarge-window :which-key "enlarge window")
 "C-M-s-<down>" '(shrink-window :which-key "shrink window")
 ) ;; End SPC prefix block

;; All-mode keymaps
(general-def
  :keymaps 'override

  ;; Emacs --------
  "M-x" 'counsel-M-x
  "C-S-B" 'counsel-switch-buffer
  "s-o" 'jb-hydra-window/body

  ;; Remapping normal help features to use Counsel version
  "C-h v" 'counsel-describe-variable
  "C-h o" 'counsel-describe-symbol
  "C-h f" 'counsel-describe-function
  "C-h F" 'counsel-describe-face

  ;; Editing ------
  "M-v" 'simpleclip-paste
  "M-c" 'simpleclip-copy
  "M-u" 'capitalize-dwim ;; Default is upcase-dwim
  "M-U" 'upcase-dwim ;; M-S-u (switch upcase and capitalize)

  ;; Utility ------
  "C-c c" 'org-capture
  "C-c a" 'org-agenda
  "C-s" 'swiper ;; Large files will use grep (faster)
  "s-\"" 'ispell-word ;; that's super-shift-'
  "M-+" 'lolo/calc-speaking-time
  "M-=" 'count-words
  "C-'" 'avy-goto-char-2
  "C-x C-b" 'bufler-list
  )

(provide 'lolo-general)
;;; lolo-general.el ends here
