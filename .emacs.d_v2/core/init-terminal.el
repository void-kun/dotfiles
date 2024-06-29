;;; init-terminal.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 vterm
 :custom (vterm-max-scrollback 100000) (vterm-buffer-name "vterm")
 :custom-face
 ;; match with fk/darken-background
 (vterm-color-default ((t (:background , nil))))
 :bind
 (:map
  vterm-mode-map
  ("C-c C-e" . vterm-copy-mode)
  ("C-c C-n" . fk/vterm-next-prompt)
  ("C-c C-p" . fk/vterm-previous-prompt)
  ;; Disabled vterm keybindings: (in order to use their global values)
  ("M-m" . nil)
  ("M-u" . nil)
  ("M-j" . nil)
  ("<f1>" . nil)
  ("C-M-s" . nil)
  :map
  vterm-copy-mode-map
  ("C-c C-e" . vterm-copy-mode)
  ("C-c C-c" . vterm-copy-mode)
  ("M-n" . fk/vterm-next-prompt)
  ("M-p" . fk/vterm-previous-prompt))
 :hook
 (vterm-mode
  .
  (lambda ()
    (setq-local
     global-hl-line-mode nil
     show-trailing-whitespace nil)))
 :config
 (defvar docker-container-prompt-regexp
   "^[\\^A-Z]*root@[A-z0-9-]*:/[^#]*# ")
 (defvar python-pdb-prompt-regexp "^[\\^A-Z]*(Pdb) ") ; TODO: add next-prompt function for this one too
 (defvar python-ipdb-prompt-regexp "^[\\^A-Z]*ipdb> "))

(use-package
 vterm-toggle
 :config
 (setq vterm-toggle-fullscreen-p nil)

 (add-to-list
  'display-buffer-alist
  '((lambda (buffer-or-name _)
      (let ((buffer (get-buffer buffer-or-name)))
        (with-current-buffer buffer
          (or (equal major-mode 'vterm-mode)
              (string-prefix-p
               vterm-buffer-name (buffer-name buffer))))))
    (display-buffer-reuse-window display-buffer-at-bottom)
    (reusable-frames . visible) (window-height . 0.3))))

(provide 'init-terminal)
;;; init-terminal.el ends here
