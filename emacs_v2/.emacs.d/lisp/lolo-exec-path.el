;;; lolo-exec-path.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:


(require-package 'exec-path-from-shell)


(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))
(exec-path-from-shell-initialize)


(provide 'lolo-exec-path)
;;; lolo-exec-path.el ends here
