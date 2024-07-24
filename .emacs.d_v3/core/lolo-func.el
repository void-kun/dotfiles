;;; lolo-func.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Define function and vars.

(eval-when-compile
  (require 'lolo-vars))

(defvar socks-noproxy)
(defvar socks-server)

(declare-function browse-url-interactive-arg "browse-url")
(declare-function chart-bar-quickie "chart")
(declare-function consult-theme "ext:consult")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")

;; ============================================================================
;; Interactive functions.

;;;;;; Remove useless whitespace before saving a file
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
  "Invoke `delete-trailing-whitespace-except-current-line' on selected.

major modes only."
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

;;;;;;

;;;;;; Resizes the window width based on the input
(defun resize-window (width delta)
  "Resize the current window's size.  If WIDTH is non-nil, resize width by some DELTA."
  (if (> (count-windows) 1)
      (window-resize nil delta width)
    (error "You need more than 1 window to execute this function!")))

(defun lolo/window-width-increase ()
  "Increase window width."
  (interactive)
  (resize-window t 5))

(defun lolo/window-width-decrease ()
  "Decrease window width."
  (interactive)
  (resize-window t -5))

(defun lolo/window-height-increase ()
  "Increase window height."
  (interactive)
  (resize-window nil 5))

(defun lolo/window-height-decrease ()
  "Decrease window height."
  (interactive)
  (resize-window nil -5))

;;;;;;

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

;;;;;;

;;;;;; Search on internet
(defun lolo/search (query-url prompt)
  "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt'."
  (browse-url
   (concat
    query-url
    (url-hexify-string
     (if mark-active
         (buffer-substring (region-beginning) (region-end))
       (read-string prompt))))))

(defmacro lolo/install-search-engine
    (search-engine-name search-engine-url search-engine-prompt)
  "Given some information regarding a search engine, install the interactive command to search through them"
  `(defun ,(intern (format "lolo-%s" search-engine-name)) ()
     ,(format "Search %s with a query or region if any."
              search-engine-name)
     (interactive)
     (lolo/search ,search-engine-url ,search-engine-prompt)))

(lolo/install-search-engine
 "google" "https://www.google.com/search?q=" "Google: ")
(lolo/install-search-engine
 "youtube"
 "https://www.youtube.com/results?search_query="
 "Youtube: ")
(lolo/install-search-engine
 "github" "https://github.com/search?q=" "Github: ")
(lolo/install-search-engine
 "duckduckgo" "https://duckduckgo.com/?t=lm&q=" "DuckDuckGo: ")

;;;;;;

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
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
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

;;;;;;

;;;;;; Proxy
(defun lolo/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" lolo-proxy)
    (message "No HTTP proxy")))

(defun lolo/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,lolo-proxy)
          ("https" . ,lolo-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (lolo/proxy-http-show))

(defun lolo/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (lolo/proxy-http-show))

(defun lolo/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (lolo/proxy-http-disable)
    (lolo/proxy-http-enable)))

(defun lolo/proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server)
               (cadr socks-server)
               (caddr socks-server))
    (message "No SOCKS proxy")))

(defun lolo/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq
   url-gateway-method 'socks
   socks-noproxy '("localhost"))
  (let* ((proxy (split-string lolo-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" lolo-socks-proxy))
  (lolo/proxy-socks-show))

(defun lolo/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq
   url-gateway-method 'native
   socks-noproxy nil
   socks-server nil)
  (setenv "all_proxy" "")
  (lolo/proxy-socks-show))

(defun lolo/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (lolo/proxy-socks-disable)
    (lolo/proxy-socks-enable)))

;;;;;;

;;;;;; Compile emacs
(defun lolo/byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun lolo/byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun lolo/native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun lolo/native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-elisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

;;;;;;

;;;;;; Frame
(defvar lolo-frame--geometry nil)
(defun lolo-frame--save-geometry ()
  "Save current frame's geometry."
  (setq lolo-frame--geometry
        `((left . ,(frame-parameter nil 'left))
          (top . ,(frame-parameter nil 'top))
          (width . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

(defun lolo-frame--fullscreen-p ()
  "Return Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun lolo-frame-maximize ()
  "Maximize the frame."
  (interactive)
  (lolo-frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun lolo-frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil lolo-frame--geometry))

(defun lolo-frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (lolo-frame--fullscreen-p)
    (lolo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun lolo-frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (lolo-frame--fullscreen-p)
    (lolo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun lolo-frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (lolo-frame--fullscreen-p)
    (lolo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun lolo-frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (lolo-frame--fullscreen-p)
    (lolo-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;;;;;;

;;;;;; Move activate window larger
(defun lolo/switch-window ()
  "Switch window and make active window larger."
  (interactive)
  (shrink-window-horizontally 45)
  (ace-window -1)
  (enlarge-window-horizontally 45))

;;;;;;

;; ============================================================================
;; Utility functions.

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun lolo/display-line-overlay+ (pos str &optional face)
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

(defun lolo/set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.
  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save) (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward (format
                                 "^[\t ]*[;]*[\t ]*(setq %s .*)"
                                 variable)
                                nil t)
        (replace-match (format "(setq %s '%s)" variable value)
                       nil
                       nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  t)

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (childframe-workable-p))

(provide 'lolo-func)
;;; lolo-func.el ends here
