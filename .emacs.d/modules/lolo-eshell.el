;;; lolo-eshell.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:


;; Emacs command shell
(use-package
 eshell
 :ensure nil
 :defines eshell-prompt-function
 :bind (:map eshell-mode-map ([remap recenter-top-bottom] . eshell/clear))
 :config
 (with-no-warnings
   (defun eshell/clear ()
     "Clear the eshell buffer."
     (interactive)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (eshell-send-input)))

   (defun eshell/emacs (&rest args)
     "Open a file (ARGS) in Emacs.  Some habits die hard."
     (if (null args)
         ;; If I just ran "emacs", I probably expect to be launching
         ;; Emacs, which is rather silly since I'm already in Emacs.
         ;; So just pretend to do what I ask.
         (bury-buffer)
       ;; We have to expand the file names or else naming a directory in an
       ;; argument causes later arguments to be looked for in that directory,
       ;; not the starting directory
       (mapc
        #'find-file (mapcar #'expand-file-name (flatten-tree (reverse args))))))
   (defalias 'eshell/e #'eshell/emacs) (defalias 'eshell/ec #'eshell/emacs)

   (defun eshell/ebc (&rest args)
     "Compile a file (ARGS) in Emacs. Use `compile' to do background make."
     (if (eshell-interactive-output-p)
         (let ((compilation-process-setup-function
                (list
                 'lambda nil
                 (list
                  'setq
                  'process-environment
                  (list 'quote (eshell-copy-environment))))))
           (compile (eshell-flatten-and-stringify args))
           (pop-to-buffer compilation-last-buffer))
       (throw 'eshell-replace-command
              (let ((l (eshell-stringify-list (flatten-tree args))))
                (eshell-parse-command (car l) (cdr l))))))
   (put 'eshell/ebc 'eshell-no-numeric-conversions t)

   (defun eshell-view-file (file)
     "View FILE.  A version of `view-file' which properly rets the eshell prompt."
     (interactive "fView file: ")
     (unless (file-exists-p file)
       (error "%s does not exist" file))
     (let ((buffer (find-file-noselect file)))
       (if (eq
            (get (buffer-local-value 'major-mode buffer) 'mode-class) 'special)
           (progn
             (switch-to-buffer buffer)
             (message "Not using View mode because the major mode is special"))
         (let ((undo-window
                (list
                 (window-buffer)
                 (window-start)
                 (+ (window-point) (length (funcall eshell-prompt-function))))))
           (switch-to-buffer buffer)
           (view-mode-enter
            (cons (selected-window) (cons nil undo-window)) 'kill-buffer)))))

   (defun eshell/less (&rest args)
     "Invoke `view-file' on a file (ARGS).

\"less +42 foo\" will go to line 42 in the buffer for foo."
     (while args
       (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
           (let* ((line (string-to-number (match-string 1 (pop args))))
                  (file (pop args)))
             (eshell-view-file file)
             (forward-line line))
         (eshell-view-file (pop args)))))
   (defalias 'eshell/more #'eshell/less))

 ;;  Display extra information for prompt
 (use-package
  eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :autoload (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init
  (setq
   eshell-highlight-prompt nil
   eshell-prompt-function #'epe-theme-lambda))

 ;; `eldoc' support
 (use-package esh-help :init (setup-esh-help-eldoc))

 ;; `cd' to frequent directory in `eshell'
 (use-package eshell-z :hook (eshell-mode . (lambda () (require 'eshell-z)))))

(provide 'lolo-eshell)
;;; lolo-eshell.el ends here
