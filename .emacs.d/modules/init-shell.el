;;; init-shell.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(require 'sh-script)

;; recognize prezto files as zsh scripts
(defvar lolo-prezto-files '("zlogin" "zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      lolo-prezto-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) lolo-prezto-files))
                (sh-set-shell "zsh"))))

(provide 'init-shell)
;;; init-shell.el ends here
