;;; lolo-ivy.el --------------------------------

(lolo-require-packages '(ivy swiper counsel))

(require 'ivy)
(require 'diminish)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

(diminish 'ivy-mode)

;; swiper
(global-set-key "\C-s" 'swiper)

;; counsel

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git) ; will override the keybinding for `magit-file-dispatch'
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c a") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(provide 'lolo-ivy)
;;; lolo-ivy.el ends here
