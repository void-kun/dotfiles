;;; lolo-general.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package general)

(general-define-key
 :states '(normal motion visual)
 :keymaps 'override
 :prefix "SPC"

 ;; Top level functions
 "/" '(lolo/rg :which-key "ripgrep")
 ";" '(spacemacs/deft :which-key "deft")
 ":" '(project-find-file :which-key "p-find file")
 "." '(counsel-find-file :which-key "find file")
 "," '(counsel-recentf :which-key "recent files")
 "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
 "SPC" '(counsel-find-file :which-key "M-x")
 "q" '(save-buffers-kill-terminal :which-key "quit emacs")
 "r" '(jump-to-register :which-key "registers")
 "c" 'org-capture

 ;; "Applications"
"a" '(nil :which-key "applications")
"ao" '(org-agenda :which-key "org-agenda")
"am" '(mu4e :which-key "mu4e")
"aC" '(calc :which-key "calc")
"ac" '(org-capture :which-key "org-capture")
"aqq" '(org-ql-view :which-key "org-ql-view")
"aqs" '(org-ql-search :which-key "org-ql-search")

"ab" '(nil :which-key "browse url")
"abf" '(browse-url-firefox :which-key "firefox")
"abc" '(browse-url-chrome :which-key "chrome")
"abx" '(xwidget-webkit-browse-url :which-key "xwidget")

"ad" '(dired :which-key "dired")

;; Buffers
"b" '(nil :which-key "buffer")
"bb" '(counsel-switch-buffer :which-key "switch buffers")
"bs" '(lolo/switch-to-scratch-buffer :which-key "scratch buffer")
"bm" '(lolo/kill-other-buffers :which-key "kill other buffers")
"bi" '(clone-indirect-buffer  :which-key "indirect buffer")
"br" '(revert-buffer :which-key "revert buffer")

;; Files
"f" '(nil :which-key "files")
"fb" '(counsel-bookmark :which-key "bookmarks")
"ff" '(counsel-find-file :which-key "find file")
"fn" '(spacemacs/new-empty-buffer :which-key "new file")
"fr" '(counsel-recentf :which-key "recent files")
"fR" '(rename-file :which-key "rename file")
"fs" '(save-buffer :which-key "save buffer")
"fo" '(reveal-in-osx-finder :which-key "reveal in finder")

;; Jake
"j" '(nil :which-key "jake")
"jb" '((lambda() (interactive) (find-file (concat lolo/dropbox "org/work.org"))) :which-key "work.org")
"jc" '((lambda() (interactive) (find-file (concat lolo/dropbox "org/cpb.org"))) :which-key "cpb.org")

"jr" '(restart-emacs :which-key "restart emacs")

"jh" '(nil :which-key "hydras")
"jht" '(jb-hydra-theme-switcher/body :which-key "themes")
"jhf" '(jb-hydra-variable-fonts/body :which-key "mixed-pitch face")
"jhw" '(jb-hydra-window/body :which-key "window control")

"jm" '(nil :which-key "macros/custom commands")
"jml" '(lolo/listify :which-key "Listify")
"jmL" '(lolo|SubListify :which-key "SubListify")
"jmo" '(lolo/org-temp-export-html :which-key "org temp export region")

"jk" '(nil :which-key "agenda/ql")
"jkq" '((lambda () (interactive) (org-ql-view "Jake Work Full View")) :which-key "jake ql")

;; Help/emacs
"h" '(nil :which-key "help/emacs")

"hv" '(counsel-describe-variable :which-key "des. variable")
"hb" '(counsel-descbinds :which-key "des. bindings")
"hM" '(describe-mode :which-key "des. mode")
"hf" '(counsel-describe-function :which-key "des. func")
"hF" '(counsel-describe-face :which-key "des. face")
"hk" '(describe-key :which-key "des. key")

"hed" '((lambda () (interactive) (jump-to-register 67)) :which-key "edit dotfile")

"hm" '(nil :which-key "switch mode")
"hme" '(emacs-lisp-mode :which-key "elisp mode")
"hmo" '(org-mode :which-key "org mode")
"hmt" '(text-mode :which-key "text mode")

"hp" '(nil :which-key "packages")
"hpr" 'package-refresh-contents
"hpi" 'package-install
"hpd" 'package-delete

;; Help/emacs
"x" '(nil :which-key "text")
"xC" '(lolo/copy-whole-buffer-to-clipboard :which-key "copy whole buffer to clipboard")
"xr" '(anzu-query-replace :which-key "find and replace")
"xs" '(yas-insert-snippet :which-key "insert yasnippet")
"xf" '(flush-lines :which-key "flush-lines")
"xR" '(replace-regexp :which-key "replace-regexp")

;; Toggles
"t" '(nil :which-key "toggles")
"tt" '(toggle-truncate-lines :which-key "truncate lines")
"tv" '(visual-line-mode :which-key "visual line mode")
"tn" '(display-line-numbers-mode :which-key "display line numbers")
"ta" '(mixed-pitch-mode :which-key "variable pitch mode")
"ty" '(counsel-load-theme :which-key "load theme")
"tw" '(writeroom-mode :which-key "writeroom-mode")
"tR" '(read-only-mode :which-key "read only mode")
"tI" '(toggle-input-method :which-key "toggle input method")
"tr" '(display-fill-column-indicator-mode :which-key "fill column indicator")
"tm" '(hide-mode-line-mode :which-key "hide modeline mode")

;; Windows
"w" '(nil :which-key "window")
"wm" '(lolo/toggle-maximize-buffer :which-key "maximize buffer")
"wN" '(make-frame :which-key "make frame")
"w-" '(lolo/split-window-vertically-and-switch :which-key "split below")
"w/" '(lolo/split-window-horizontally-and-switch :which-key "split right")
"wr" '(jb-hydra-window/body :which-key "hydra window")
"wz" '(text-scale-adjust :which-key "text zoom")
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

  ;; super-number functions
  "s-1" 'mw-thesaurus-lookup-dwim
  "s-!" 'mw-thesaurus-lookup
  "s-2" 'ispell-buffer
  "s-3" 'revert-buffer
  "s-4" '(lambda () (interactive) (counsel-file-jump nil lolo/dropbox))
  "s-5" '(lambda () (interactive) (counsel-rg nil lolo/dropbox))
  "s-6" 'org-capture

  "s-w" 'kill-this-buffer
  )

  ;; Non-insert mode keymaps
(general-def
  :states '(normal visual motion)
  "gc" 'comment-dwim
  "gC" 'comment-line

  "u" 'undo-fu-only-undo
  "U" 'undo-fu-only-redo

  "gf" 'xah-open-file-at-cursor

  "/" 'lolo/split-window-horizontally-and-switch
  "-" 'lolo/split-window-vertically-and-switch  

  "\\" '(lambda () (interactive) (org-agenda nil "c"))
  "|" '(lambda () (interactive) (org-ql-view "Columbia Todo"))
  "]\\" '(lambda () (interactive) (org-agenda nil "w"))
  )

(general-def
  :states '(normal visual motion)
  :keymaps 'override
  "s" 'swiper)

(general-def
 :keymaps 'emacs
  "C-w C-q" 'kill-this-buffer
 )
(provide 'lolo-general)
;;; lolo-general.el ends here
