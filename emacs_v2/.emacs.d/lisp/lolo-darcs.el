;;; lolo-darcs.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

;; TODO: include this in the vc-darcs ELPA package
(when (maybe-require-package 'vc-darcs)
    (add-to-list 'vc-handled-backends 'DARCS)
    (autoload 'vc-darcs-find-file-hook "vc-darcs")
    (add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

    (setq darcsum-whatsnew-switches "-l"))

(provide 'lolo-darcs)
;;; lolo-darcs.el ends here
