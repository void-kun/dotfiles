;;; init-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'electric)

;; Code navigation, documentation lookup and completion for Python
(lolo/require-package 'anaconda-mode)

(when (boundp 'company-backends)
  (lolo/require-package 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(defcustom lolo/python-mode-set-encoding-automatically nil
  "Non-nil values enable auto insertion of '# coding: utf-8' on python buffers."
  :type 'boolean
  :group 'lolo)

;;; Encoding detection/insertion logic
;;
;; Adapted from ruby-mode.el
;;
;; This logic was useful in Python 2, but it's not really needed in Python 3.
(defun lolo/python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun lolo/python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun lolo/python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun lolo/python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (lolo/python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (lolo/python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (lolo/python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))

;;; python-mode setup

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defun lolo/python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (anaconda-mode +1)
  (eldoc-mode +1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (when lolo/python-mode-set-encoding-automatically
    (add-hook 'after-save-hook 'lolo/python-mode-set-encoding nil 'local)))

(setq lolo-python-mode-hook 'lolo/python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'lolo-python-mode-hook)))

(provide 'init-python)
;;; init-python.el ends here
