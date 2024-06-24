;;; init-funs.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

(defun toggle-auto-trailing-ws-removal ()
  "Toggle trailing whitespace removal."
  (interactive)
  (if (member #'smart-delete-trailing-whitespace before-save-hook)
      (progn
        (remove-hook
         'before-save-hook #'smart-delete-trailing-whitespace)
        (message "Disabled auto remove trailing whitespace."))
    (add-hook 'before-save-hook #'smart-delete-trailing-whitespace)
    (message "Enabled auto remove trailing whitespace.")))
;; Add to hook during startup
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;; Replace selection on insert
(delete-selection-mode 1)

(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties
              '(read-only
                t
                point-entered
                minibuffer-avoid-prompt
                face
                minibuffer-prompt))

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol
         (save-excursion
           (goto-char pos)
           (make-overlay
            (line-beginning-position) (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put
     ol 'face (or face '(:background null :inherit highlight)))
    ol))

(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

;; ============================================================================
;; Resize window width/height functions.
;; Resizes the window width based on the input
(defun resize-window-width (w)
  "Resizes the window width based on W."
  (interactive
   (list
    (if (> (count-windows) 1)
        (read-number "Set the current window width in [1~9]x10%: ")
      (error
       "You need more than 1 window to execute this function!"))))
  (message "%s" w)
  (window-resize nil
                 (- (truncate (* (/ w 10.0) (frame-width)))
                    (window-total-width))
                 t))

;; Resizes the window height based on the input
(defun resize-window-height (h)
  "Resizes the window height based on H."
  (interactive
   (list
    (if (> (count-windows) 1)
        (read-number "Set the current window height in [1~9]x10%: ")
      (error
       "You need more than 1 window to execute this function!"))))
  (message "%s" h)
  (window-resize nil
                 (- (truncate (* (/ h 10.0) (frame-height)))
                    (window-total-height))
                 nil))

;; Setup shorcuts for window resize width and height
(global-set-key (kbd "C-z w") #'resize-window-width)
(global-set-key (kbd "C-z h") #'resize-window-height)

(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

;; Setup shorcuts for window resize width and height
(defun window-width-increase ()
  (interactive)
  (resize-window t 5))

(defun window-width-decrease ()
  (interactive)
  (resize-window t -5))

(defun window-height-increase ()
  (interactive)
  (resize-window nil 5))

(defun window-height-decrease ()
  (interactive)
  (resize-window nil -5))

(global-set-key (kbd "M-W =") #'window-width-increase)
(global-set-key (kbd "M-W M-+") #'window-width-increase)
(global-set-key (kbd "M-W -") #'window-width-decrease)
(global-set-key (kbd "M-W M-_") #'window-width-decrease)

(global-set-key (kbd "M-Q =") #'window-height-increase)
(global-set-key (kbd "M-Q M-+") #'window-height-increase)
(global-set-key (kbd "M-Q -") #'window-height-decrease)
(global-set-key (kbd "M-Q M-_") #'window-height-decrease)


(provide 'init-funs)
;;; init-funs.el ends here
