;;; lolo-erc.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; open query buffers in the current window
(setq erc-query-display 'buffer)

;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(setq erc-save-buffer-on-part t)
;; FIXME - this advice is wrong and is causing problems on Emacs exit
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(when lolo-flyspell
  (erc-spelling-mode 1))
;; set different dictionaries by different servers/channels
;;(setq erc-spelling-dictionaries '(("#emacs" "american")))

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")


(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from
the same person.")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

;; autoaway setup
(setq erc-auto-discard-away t)
(setq erc-autoaway-idle-seconds 600)
(setq erc-autoaway-use-emacs-idle t)
;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

(defvar my-fav-irc '( "irc.freenode.net" )
  "Stores the list of IRC servers that you want to connect to with start-irc.")

(defvar bye-irc-message "Asta la vista"
  "Message string to be sent while quitting IRC.")

(defcustom lolo-new-irc-persp nil
  "True (t) means start IRC in new perspective."
  :type 'boolean
  :require 'lolo-erc
  :group 'lolo)

(defun connect-to-erc (server)
  "Connects securely to IRC SERVER over TLS at port 6697."
  (erc-tls :server server
           :port 6697
           :nick erc-nick ))

(defun start-irc ()
  "Connect to IRC?"
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (when lolo-new-irc-persp
      (persp-switch "IRC"))
    (mapcar 'connect-to-erc my-fav-irc)))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers."
  (interactive)
  (when lolo-new-irc-persp
    (persp-switch "IRC"))
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server bye-irc-message)))
  (when lolo-new-irc-persp
    (persp-kill "IRC")))

(provide 'lolo-erc)
;;; lolo-erc.el ends here