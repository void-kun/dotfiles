;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package
 shell
 :hook
 ((shell-mode . my-shell-mode-hook)
  (shell-mode . tree-sitter-hl-mode)
  (comint-output-filter-functions . comint-strip-ctrl-m))
 :init (setq system-uses-terminfo nil)
 (defun my-shell-simple-send (proc command)
   "Various PROC COMMANDs pre-processing before sending to shell."
   (cond
    ;; Checking for clear command and execute it.
    ((string-match "^[ \t]*clear[ \t]*$" command)
     (comint-send-string proc "\n")
     (erase-buffer))
    ;; Checking for man command and execute it.
    ((string-match "^[ \t]*man[ \t]*" command)
     (comint-send-string proc "\n")
     (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
     (setq command (replace-regexp-in-string "[ \t]+$" "" command))
     ;;(message (format "command %s command" command))
     (funcall 'man command))
    ;; Send other commands to the default handler.
    (t
     (comint-simple-send proc command))))

 (defun my-shell-mode-hook ()
   "Shell mode customization."
   (local-set-key '[up] 'comint-previous-input)
   (local-set-key '[down] 'comint-next-input)
   (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)

   (ansi-color-for-comint-mode-on)
   (setq comint-input-sender 'my-shell-simple-send)))

;; ============================================================================
;; Better terminal emulator
(when (and module-file-suffix ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool") ; libtool-bin
           (executable-find "make"))
  (use-package vterm :init (setq vterm-always-compile-module t))

  (use-package
   multi-vterm
   :custom (multi-vterm-buffer-name "vterm")
   :config
   (with-no-warnings
     ;; Use `pop-to-buffer' instead of `switch-to-buffer'
     (defun my-multi-vterm ()
       "Create new vterm buffer."
       (interactive)
       (let ((vterm-buffer (multi-vterm-get-buffer)))
         (setq multi-vterm-buffer-list
               (nconc multi-vterm-buffer-list (list vterm-buffer)))
         (set-buffer vterm-buffer)
         (multi-vterm-internal)
         (pop-to-buffer vterm-buffer)))
     (advice-add #'multi-vterm :override #'my-multi-vterm))))

;; Shell Pop: leverage `popper'
(defvar shell-pop--frame nil)
(defvar shell-pop--window nil)

(defun shell-pop--shell (&optional arg)
  "Run shell and return the buffer."
  (cond
   ((fboundp 'vterm)
    (vterm arg))
   (t
    (shell))))

(defun shell-pop--hide-frame ()
  "Hide child frame and refocus in parent frame."
  (when (and (frame-live-p shell-pop--frame) (frame-visible-p shell-pop--frame))
    (make-frame-invisible shell-pop--frame)
    (select-frame-set-input-focus (frame-parent shell-pop--frame))
    (setq shell-pop--frame nil)))

(defun shell-pop-toggle ()
  "Toggle shell."
  (interactive)
  (shell-pop--hide-frame)
  (if (window-live-p shell-pop--window)
      (progn
        (delete-window shell-pop--window)
        (setq shell-pop--window nil))
    (setq shell-pop--window (get-buffer-window (shell-pop--shell)))))

(bind-key "C-<escape>" #'shell-pop-toggle)

(provide 'lolo-terminal)
;;; lolo-terminal.el ends here
