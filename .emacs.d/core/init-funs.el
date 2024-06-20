;;; init-funs.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
    (require 'cl-lib)
    (require 'init-custom))

(defvar socks-noproxy)
(defvar socks-server)

(declare-function browse-url-interactive-arg "browse-url")
(declare-function chart-bar-quickie "chart")
(declare-function consult-theme "ext:consult")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")

(defun lolo/buffer-mode (buffer-or-name)
    "Retrieve the `major-mode' or BUFFER-OR-NAME."
    (with-current-buffer buffer-or-name
        major-mode))

(defun lolo/search (query-url prompt)
    "Open the search url constructed with the QUERY-URL.
PROMPT sets the `read-string prompt'."
    (browse-url
        (concat query-url
                (url-hexify-string
                    (if mark-active
                        (buffer-substring (region-beginning) (region-end))
                        (read-string prompt))))))

(defmacro lolo/install-search-engine (search-engine-name search-engine-url search-engine-prompt)
    "Given some information regarding a search engine, install the interactive command to search through them"
    `(defun ,(intern (format "lolo-%s" search-engine-name)) ()
        ,(format "Search %s with a query or region if any." search-engine-name)
        (interactive)
        (lolo/search ,search-engine-url ,search-engine-prompt)))

(lolo/install-search-engine "google"        "https://www.google.com/search?q="              "Google: ")
(lolo/install-search-engine "youtube"       "https://www.youtube.com/results?search_query=" "Youtube: ")
(lolo/install-search-engine "github"        "https://github.com/search?q="                  "Github: ")
(lolo/install-search-engine "duckduckgo"    "https://duckduckgo.com/?t=lm&q="               "DuckDuckGo: ")

(defun lolo/wrap-with (s)
    "create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
        (interactive "P")
        (sp-wrap-with-pair ,s)))

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

(defun lolo/mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3) (elt (window-inside-pixel-edges) 3)
     (if (bound-and-true-p window-divider-mode)
         window-divider-default-bottom-width
       0)))

(defun lolo/reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'lolo/reload-init-file #'reload-init-file)

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
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun lolo/set-variable (variable value &optional no-save)
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

(provide 'init-funs)
;;; init-funs.el ends here
