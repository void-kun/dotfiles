;;; lolo-web.el --------------------------------


;;; Code:
;; WebModePac
(use-package web-mode
  :custom-face
  (css-selector ((t (:inherit default :foreground "#66CCFF"))))
  (font-lock-comment-face ((t (:foreground "#828282"))))
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'"))
;; -WebModePac

;; Js2Pac
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :bind (:map js-mode-map ("M-." . nil)))
;; -Js2Pac

;; TypeScriptPac
(use-package typescript-mode
  :mode "\\.ts\\'"
  :commands (typescript-mode))
;; -TypeScriptPac

;; VuePac
(use-package vue-mode
  :mode "\\.vue\\'"
  :commands (vue-mode))
;; -VuePac


;; EmmetPac
(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (css-mode . emmet-mode)))
;; -EmmetPac

;; JsonPac
(use-package json-mode
  :mode "\\.json\\'")
;; -JsonPac

(provide 'lolo-web)
;;; lolo-web.el ends here
