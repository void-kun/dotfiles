;;; lolo-editor.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Multiple cursor.
(use-package multiple-cursors)
(multiple-cursors-mode +1)

;;; Colorful mode
(use-package colorful-mode :ensure t :hook (prog-mode text-mode))

;;; spell checker
(use-package
 flycheck
 :defer t
 :diminish
 :hook (after-init . global-flycheck-mode)
 :commands (flycheck-add-mode)
 :custom
 (flycheck-global-modes
  '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
 (flycheck-emacs-lisp-load-path 'inherit)
 (flycheck-indication-mode
  (if (display-graphic-p)
      'right-fringe
    'right-margin))
 :init
 (if (display-graphic-p)
     (use-package
      flycheck-posframe
      :custom-face
      (flycheck-posframe-face
       ((t (:foreground ,(face-foreground 'success)))))
      (flycheck-posframe-info-face
       ((t (:foreground ,(face-foreground 'success)))))
      :hook (flycheck-mode . flycheck-posframe-mode)
      :custom
      (flycheck-posframe-position 'window-bottom-left-corner)
      (flycheck-posframe-border-width 3)
      (flycheck-posframe-inhibit-functions
       '((lambda (&rest _) (bound-and-true-p company-backend)))))
   (use-package
    flycheck-pos-tip
    :defines flycheck-pos-tip-timeout
    :hook (flycheck-mode . flycheck-pos-tip-mode)
    :custom (flycheck-pos-tip-timeout 30)))
 :config
 (use-package
  flycheck-popup-tip
  :hook (flycheck-mode . flycheck-popup-tip-mode))
 (when (fboundp 'define-fringe-bitmap)
   (define-fringe-bitmap
     'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16]
     nil nil 'center))
 (when (executable-find "vale")
   (use-package
    flycheck-vale
    :config
    (flycheck-vale-setup)
    (flycheck-add-mode 'vale 'latex-mode))))

(use-package
 flyspell
 :ensure nil
 :diminish
 :if (executable-find "aspell")
 :hook
 (((text-mode outline-mode latex-mode org-mode markdown-mode)
   . flyspell-mode))
 :custom
 (flyspell-issue-message-flag nil)
 (ispell-program-name "aspell")
 (ispell-extra-args
  '("--sug-mode=ultra" "--lang=en_US" "--camel-case")))

(use-package
 smartparens
 :hook (prog-mode . smartparens-mode)
 :diminish smartparens-mode
 :custom (sp-escape-quotes-after-insert nil)
 :config
 ;; stop pairing single quotes in elisp
 (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
 (sp-local-pair 'org-mode "[" nil :actions nil))

;; show matching parenthesis
(show-paren-mode 1)
;; we will call `blink-matching-open` ourselves...
(remove-hook
 'post-self-insert-hook #'blink-paren-post-self-insert-function)

;; this still needs to be set for `blink-matching-open` to work
(setq blink-matching-paren 'show)
(let ((ov nil)) ; keep track of the overlay
  (advice-add
   #'show-paren-function
   :after
   (defun show-paren--off-screen+ (&rest _args)
     "display matching line for off-screen paren."
     (when (overlayp ov)
       (delete-overlay ov))
     ;; check if it's appropriate to show match info,
     ;; see `blink-paren-post-self-insert-function'
     (when (and (overlay-buffer show-paren--overlay)
                (not
                 (or cursor-in-echo-area
                     executing-kbd-macro
                     noninteractive
                     (minibufferp)
                     this-command))
                (and (not (bobp))
                     (memq (char-syntax (char-before)) '(?\) ?\$)))
                (= 1
                   (logand
                    1
                    (- (point)
                       (save-excursion
                         (forward-char -1)
                         (skip-syntax-backward "/\\")
                         (point))))))
       ;; rebind `minibuffer-message' called by
       ;; `blink-matching-open' to handle the overlay display
       (cl-letf (((symbol-function #'minibuffer-message)
                  (lambda (msg &rest args)
                    (let ((msg (apply #'format-message msg args)))
                      (setq ov
                            (display-line-overlay+
                             (window-start) msg))))))
         (blink-matching-open))))))

(use-package
 ediff
 :custom
 (ediff-split-window-function #'split-window-horizontally)
 (ediff-window-setup-function #'ediff-setup-windows-plain))

;; ============================================================================
;; Iedit, a minor mode that allows editing multiple regions simultaneousy in a buffer or a region.
(use-package iedit :bind ("C-z ," . iedit-mode) :diminish)

(provide 'lolo-editor)
;;; lolo-editor.el ends here
