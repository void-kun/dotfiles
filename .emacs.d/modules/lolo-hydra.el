;;; lolo-hydra.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package hydra :defer t)

;; This Hydra lets me swich between variable pitch fonts.
(defhydra jb-hydra-variable-fonts (:pre (mixed-pitch-mode 0)
                                     :post (mixed-pitch-mode 1))
  ("t" (set-face-attribute 'variable-pitch nil :family "Times New Roman" :height 160) "Times New Roman")
  ("g" (set-face-attribute 'variable-pitch nil :family "EB Garamond" :height 160 :weight 'normal) "EB Garamond")
  ("n" (set-face-attribute 'variable-pitch nil :slant 'normal :weight 'normal :height 160 :width 'normal :foundry "nil" :family "Nunito") "Nunito")
  )

(defhydra jb-hydra-theme-switcher (:hint nil)
  "
     Dark                ^Light^
----------------------------------------------
_1_ one              _z_ one-light 
_2_ vivendi          _x_ operandi
_3_ molokai          _c_ jake-plain
_4_ snazzy           _v_ flatwhite
_5_ old-hope         _b_ tomorrow-day
_6_ henna                ^
_7_ kaolin-galaxy        ^
_8_ peacock              ^
_9_ jake-plain-dark      ^
_0_ monokai-machine      ^
_-_ xcode                ^
_q_ quit                 ^
^                        ^
"

  ;; Dark
  ("1" (lolo/load-theme 'doom-one)				 "one")
  ("2" (lolo/load-theme 'modus-vivendi)			 "modus-vivendi")
  ("3" (lolo/load-theme 'doom-molokai)			 "molokai")
  ("4" (lolo/load-theme 'doom-snazzy)			 "snazzy")
  ("5" (lolo/load-theme 'doom-old-hope)			 "old-hope")
  ("6" (lolo/load-theme 'doom-henna)				 "henna")
  ("7" (lolo/load-theme 'kaolin-galaxy)			 "kaolin-galaxy")
  ("8" (lolo/load-theme 'doom-peacock)			 "peacock")
  ("9" (lolo/load-theme 'jake-doom-plain-dark)	 "jake-plain-dark")
  ("0" (lolo/load-theme 'doom-monokai-machine)	 "monokai-machine")
  ("-" (lolo/load-theme 'doom-xcode)				 "xcode")

  ;; Light
  ("z" (lolo/load-theme 'doom-one-light)			 "one-light")
  ("x" (lolo/load-theme 'modus-operandi)			 "modus-operandi")
  ("c" (lolo/load-theme 'jake-doom-plain)		 "jake-plain")
  ("v" (lolo/load-theme 'doom-flatwhite)			 "flatwhite")
  ("b" (lolo/load-theme 'doom-opera-light)		 "tomorrow-day")
  ("q" nil))

;; I think I need to initialize windresize to use its commands
;;(windresize)
;;(windresize-exit)

;;(require 'windresize)

;; All-in-one window managment. Makes use of some custom functions,
;; `ace-window' (for swapping), `windmove' (could probably be replaced
;; by evil?) and `windresize'.
;; inspired by https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L86
(defhydra jb-hydra-window (:hint nil)
   "
Movement      ^Split^            ^Switch^        ^Resize^
----------------------------------------------------------------
_M-<left>_  <   _/_ vertical      _b_uffer        _<left>_  <
_M-<right>_ >   _-_ horizontal    _f_ind file     _<down>_  ↓
_M-<up>_    ↑   _m_aximize        _s_wap          _<up>_    ↑
_M-<down>_  ↓   _c_lose           _[_backward     _<right>_ >
_q_uit          _e_qualize        _]_forward     ^
^               ^               _K_ill         ^
^               ^                  ^             ^
"
   ;; Movement
   ("M-<left>" windmove-left)
   ("M-<down>" windmove-down)
   ("M-<up>" windmove-up)
   ("M-<right>" windmove-right)

   ;; Split/manage
   ("-" lolo/split-window-vertically-and-switch)
   ("/" lolo/split-window-horizontally-and-switch)
   ("m" delete-other-windows)
   ("e" balance-windows)

   ;; Switch
   ("b" counsel-switch-buffer)
   ("f" counsel-find-file)
   ("P" project-find-file)
   ("s" ace-swap-window)
   ("[" previous-buffer)
   ("]" next-buffer)
   ("K" kill-this-buffer)

   ;; Resize
   ("<left>" windresize-left)
   ("<right>" windresize-right)
   ("<down>" windresize-down)
   ("<up>" windresize-up)

   ("q" nil))

(defhydra jb-hydra-org-table ()
  "
_c_ insert col    _v_ delete col    Move col: _h_, _l_
_r_ insert row    _d_ delete row    Move row: _j_, _k_
_n_ create table  _i_ create hline
_u_ undo
_q_ quit

"
  ("n" org-table-create "create table")
  ("c" org-table-insert-column "insert col")
  ("r" org-table-insert-row "insert row")
  ("v" org-table-delete-column "delete col")
  ("d" org-table-kill-row "delete row")
  ("i" org-table-insert-hline "hline")

  ("u" undo-fu-only-undo "undo")

  ("h" org-table-move-column-left "move col left")
  ("l" org-table-move-column-right "move col right")
  ("k" org-table-move-row-up "move row up")
  ("j" org-table-move-row-down "move row down")

  ("<left>" org-table-previous-field)
  ("<right>" org-table-next-field)
  ("<up>" previous-line)
  ("<down>" org-table-next-row)

  ("q" nil "quit"))

(provide 'lolo-hydra)
;;; lolo-hydra.el ends here
