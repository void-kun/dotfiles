;; lolo-ui.el ;--------------------------------

;;; Code:
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(menu-bar-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)
;; disable startup screen
(setq inhibit-startup-screen t)
;; config font settings
(custom-set-faces
 '(default ((t (:family "IosevkaLyteTerm" :foundry "UKWN" :slant normal :weight regular :height 120 :width normal)))))
;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Set padding between line
(defvar line-padding 2)
(defun add-line-padding ()
  "Add extra padding between lines."

  ; remove padding overlays if they already exist
  (let ((overlays (overlays-at (point-min))))
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay 'is-padding-overlay)
            (delete-overlay overlay)))
      (setq overlays (cdr overlays))))

  ; add a new padding overlay
  (let ((padding-overlay (make-overlay (point-min) (point-max))))
    (overlay-put padding-overlay 'is-padding-overlay t)
    (overlay-put padding-overlay 'line-spacing (* .1 line-padding))
    (overlay-put padding-overlay 'line-height (+ 1 (* .1 line-padding))))
  (setq mark-active nil))
(add-hook 'buffer-list-update-hook 'add-line-padding)

;; show line numbers at the beginning of each line
(if (fboundp 'global-display-line-numbers-mode)
    (global-display-line-numbers-mode)
   ;; (display-line-numbers-type 'relative)
    (global-nlinum-mode t))

;; enable y/n answers
(fset 'yes-nor-no-p 'y-or-n-p)

;; more useful frame title
(setq frame-title-format
    '("" invocation-name " Lolo - " (:eval (if (buffer-file-name)
                                        (abbreviate-file-name (buffer-file-name))
                                        "%b"))))
(when lolo-theme
    (load-theme lolo-theme t))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'which-key-mode)
    (which-key-mode +1))

;; Highlight focus
(add-hook 'after-init-hook (lambda ()
                             (when (fboundp 'auto-dim-other-buffers-mode)
                               (auto-dim-other-buffers-mode t))))

;; Config ui
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq split-height-threshold 0)
(setq compilation-window-height 10)

(defun my-compilation-hook ()
  "Change the window height of compilation."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")
          (shrink-window (- h compilation-window-height)))))))
(add-hook 'compilation-mode-hook 'my-compilation-hook)

(provide 'lolo-ui)
;;; lolo-ui.el ends here
