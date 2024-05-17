;;; lolo-funcs.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'cl-lib)

;; Suppress warnings
(eval-when-compile
  (require 'lolo-const)
  (require 'lolo-custom))

(defvar socks-noproxy)
(defvar socks-server)

(declare-function browse-url-interactive-arg "browse-url")
(declare-function chart-bar-quickie "chart")
(declare-function consult-theme "ext:consult")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-dos-eol ()
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


;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p
         (format "Really delete '%s'?"
                 (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
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

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p) (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
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

;; Browse URL
(defun lolo-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

  POP-BUFFER specifies whether to pop to the buffer.
  NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))

  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3) (elt (window-inside-pixel-edges) 3)
     (if (bound-and-true-p window-divider-mode)
         window-divider-default-bottom-width
       0)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'lolo-reload-init-file #'reload-init-file)

;; Open custom file
(defun find-custom-file ()
  "Open custom files."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p lolo-custom-example-file)
        (copy-file lolo-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" lolo-custom-example-file)))
  (when (file-exists-p custom-file)
    (find-file custom-file))
  (when (file-exists-p lolo-custom-post-file)
    (find-file-other-window lolo-custom-post-file)))

;; Misc
(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and lolo-icon (or (featurep 'nerd-icons) (require 'nerd-icons nil t))))

(defun lolo-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and lolo-tree-sitter (fboundp 'treesit-available-p) (treesit-available-p)))

(defun lolo-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save) (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward (format "^[\t ]*[;]*[\t ]*(setq %s .*)"
                                        variable)
                                nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(define-minor-mode lolo-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group
  lolo
  (if lolo-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +1))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to option `custom-file' if NO-SAVE is nil."
  (interactive (list
                (intern
                 (completing-read
                  "Select package archives: "
                  (mapcar #'car lolo-package-archives-alist)))))
  ;; Set option
  (lolo-set-variable 'lolo-package-archives archives no-save)
  ;; Refresh if need
  (and refresh (package-refresh-contents async))
  (message "Set package archives to `%s'" archives))

(defalias 'lolo-set-package-archives #'set-package-archives)

(defun lolo-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations
          (mapcar
           (lambda (pair)
             (let ((url (concat (cdr (nth 2 (cdr pair))) "archive-contents"))
                   (start (current-time)))
               (message "Fetching %s..." url)
               (ignore-errors
                 (url-copy-file url null-device t))
               (float-time (time-subtract (current-time) start))))
           lolo-package-archives-alist))
         (fastest
          (car
           (nth
            (cl-position (apply #'min durations) durations)
            lolo-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart) (require 'chart nil t) (require 'url nil t))
      (chart-bar-quickie
       'vertical "Speed test for the ELPA mirrors"
       (mapcar
        (lambda (p) (symbol-name (car p))) lolo-package-archives-alist)
       "ELPA" (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'lolo-update-packages #'update-packages)

;; Fonts
(defun lolo-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (nerd-icons-install-fonts))

;; (defun childframe-workable-p ()
;;   "Whether childframe is workable."
;;   (not (or noninteractive emacs-basic-display (not (display-graphic-p)))))

(defun childframe-workable-p ()
  "Whether childframe is workable."
  t)

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq lolo-completion-style 'childframe) (childframe-workable-p)))

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

;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" lolo-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,lolo-proxy)
          ("https" . ,lolo-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server)
               (cadr socks-server)
               (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
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
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq
   url-gateway-method 'native
   socks-noproxy nil
   socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

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

(defun lolo/newline-with-indent-below ()
  "Insert newline with indent below."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun lolo/newline-with-indent-above ()
  "Insert newline with indent above."
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))

(defun lolo/kill-line ()
  "kill current line."
  (interactive)
  (beginning-of-line)
  (kill-line))

(provide 'lolo-funcs)
;;; lolo-funcs.el ends here
