;;; lolo-gui-frames.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Suppress GUI features

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)


;; Window size and features

(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(custom-set-faces
 '(default ((t (:family "RobotoMono Nerd Font" :foundry "GOOG" :slant normal :weight regular :height 120 :width normal)))))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen)

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(when (maybe-require-package 'ns-auto-titlebar)
(ns-auto-titlebar-mode))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;; Change global font size easily

(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)


(require-package 'disable-mouse)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))


(provide 'lolo-gui-frames)
;;; lolo-gui-frames.el ends here
