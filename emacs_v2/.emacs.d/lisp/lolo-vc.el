;;; lolo-vc.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(when (maybe-require-package 'diff-hl)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (add-hook 'after-init-hook 'global-diff-hl-mode)

  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map (kbd "<left-fringe> <mouse-1>") 'diff-hl-diff-goto-hunk)
    (define-key diff-hl-mode-map (kbd "M-C-]") 'diff-hl-next-hunk)
    (define-key diff-hl-mode-map (kbd "M-C-[") 'diff-hl-previous-hunk)))

(provide 'lolo-vc)
;;; lolo-vc.el ends here
