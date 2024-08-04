;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-customs))

;; ============================================================================
;; Interactive functions.

;;;;;; Move text
(defun lolo/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun lolo/move-text-down (arg)
  "Move region (transient-mark-mode active) or current line down."
  (interactive "*p")
  (lolo/move-text-internal arg))

(defun lolo/move-text-up (arg)
  "Move region (transient-mark-mode active) or current line up."
  (interactive "*p")
  (lolo/move-text-internal (- arg)))

(defun lolo/backward-kill-word ()
  "Remove all whitespace if the character behind the cursor is whitespace.
Otherwhise remove a word."
  (interactive)
  (if (looking-back "[ \n\t]")
      (progn
        (delete-horizontal-space 't)
        (while (looking-back "[ \n\t]")
          (backward-delete-char 1)))
    (backward-kill-word 1)))

;;;;;; Encoding, file and buffer
(defun lolo/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun lolo/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun lolo/delete-dos-eol ()
  "Delete `' characters in current region or buffer.
Same as '`replace-string' `C-q' `C-m' `RET' `RET''."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\r" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen)))

(defun lolo/revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun lolo/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun lolo/rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun lolo/open-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun lolo/copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun lolo/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun lolo/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "Coding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun lolo/save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun lolo/open-marked-files (&optional files)
  "Open all marked FILES in Dired buffer as new Emacs buffers."
  (interactive)
  (let* ((file-list
          (if files
              (list files)
            (if (equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name))))))
    (mapc (lambda (file-path) (find-file file-path)) (file-list))))

(defun lolo/split-window-right-and-focus ()
  "Spawn a new window right of the current one and focus it."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun lolo/split-window-below-and-focus ()
  "Spawn a new window below the current one and focus it."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun lolo/kill-buffer-and-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (progn
    (kill-this-buffer)
    (delete-window)))

(defun lolo/self-screenshot (&optional type)
  "Save a screenshot of type TYPE of the current Emacs frame.
As shown by the function `', type can wield the value `svg',
`png', `pdf'.

This function will output in /tmp a file beginning with \"Emacs\"
and ending with the extension of the requested TYPE."
  (interactive (list
                (intern
                 (completing-read
                  "Screenshot type: " '(png svg pdf postscript)))))
  (let* ((extension
          (pcase type
            ('png ".png")
            ('svg ".svg")
            ('pdf ".pdf")
            ('postscript ".ps")
            (otherwise
             (error "Cannot export screenshot of type %s" otherwise))))
         (filename (make-temp-file "Emacs-" nil extension))
         (data (x-export-frames nil type)))
    (with-temp-file filename
      (insert data))
    (kill-new filename)
    (message filename)))

(defun lolo/switch-to-messages-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer (messages-buffer)))

(defun lolo/switch-to-scratch-buffer ()
  "Switch to Messages buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defvar lolo-emacs-org-config-directory
  (expand-file-name "org/config/docs/emacs" (getenv "HOME"))
  "Location of my config as org files.")

(defun lolo/tangle-emacs-config ()
  "Tangle all my Emacs config files from org files."
  (interactive)
  (let ((files
         (f-files
          lolo-emacs-org-config-directory
          (lambda (file) (f-ext-p file "org"))
          t))
        (org-confirm-babel-evaluate nil))
    (dolist (file files)
      (message "Tangling %s" file)
      (org-babel-tangle-file file))))

(defun lolo/zoom-in (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
         (font (face-attribute 'default :font frame))
         (size (font-get font :size))
         (amt (or amt 1))
         (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun lolo/zoom-out (&optional amt frame)
  "Call `lolo/zoom-in' with negative argument."
  (interactive "p")
  (lolo/zoom-in (- (or amt 1)) frame))

;;;;;; Move activate window larger
(defun lolo/switch-window ()
  "Switch window and make active window larger."
  (interactive)
  (shrink-window-horizontally 45)
  (other-window)
  (enlarge-window-horizontally 45))

;; ============================================================================
;; Utility functions.

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun lolo/set-font ()
  (when (font-installed-p lolo/default-font-name)
    (set-face-attribute 'default nil
                        :font lolo/default-font-name
                        :height lolo/default-font-size)))

(defun lolo/read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defun lolo/add-all-to-list (list-var elements &optional append compare-fn)
  "Add ELEMENTS to the value of LIST-VAR if it isn’t there yet.

ELEMENTS is a list of values. For documentation on the variables
APPEND and COMPARE-FN, see `add-to-list'."
  (let (return)
    (dolist (elt elements return)
      (setq return (add-to-list list-var elt append compare-fn)))))

;; ============================================================================
;; Utility macros.

(defmacro csetq (&rest forms)
  "Bind each custom variable FORM to the value of its VAL.

FORMS is a list of pairs of values [FORM VAL].
`customize-set-variable' is called sequentially on each pair
contained in FORMS. This means `csetq' has a similar behaviour as
`setq': each VAL expression is evaluated sequentially, i.e. the
first VAL is evaluated before the second, and so on. This means
the value of the first FORM can be used to set the second FORM.

The return value of `csetq' is the value of the last VAL.

\(fn [FORM VAL]...)"
  (declare (debug (&rest sexp form)) (indent 1))
  ;; Check if we have an even number of arguments
  (when (= (mod (length forms) 2) 1)
    (signal 'wrong-number-of-arguments (list 'csetq (1+ (length forms)))))
  ;; Transform FORMS into a list of pairs (FORM . VALUE)
  (let (sexps)
    (while forms
      (let ((form (pop forms))
            (value (pop forms)))
        (push `(customize-set-variable ',form ,value) sexps)))
    `(progn
       ,@
       (nreverse sexps))))

(provide 'lolo-functions)
;;; lolo-functions.el ends here
