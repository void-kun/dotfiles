;;; lolo-dired.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(eval-when-compile
  (require 'lolo-const))

;; Directory operations
(use-package
  dired
  :ensure nil
  :bind (:map dired-mode-map ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)

  ;; Always delete and copy recursively
  (setq
   dired-recursive-deletes 'always
   dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  ;; Quick sort dired buffers via hydra
  (use-package
    dired-quick-sort
    :bind (:map dired-mode-map ("S" . hydra-dired-quick-sort/body)))

  ;; Show git info in dired
  (use-package
    dired-git-info
    :bind (:map dired-mode-map (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync :bind (:map dired-mode-map ("C-c C-r" . dired-rsync)))

  ;; Colorful dired
  (use-package diredfl :hook (dired-mode . diredfl-mode))

  ;; Shows icons
  (use-package
    nerd-icons-dired
    :diminish
    :when (icons-displayable-p)
    :custom-face
    (nerd-icons-dired-dir-face
     ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
    :hook (dired-mode . nerd-icons-dired-mode))

  ;; Extra Dired functionality
  (use-package dired-aux :ensure nil)
  (use-package
    dired-x
    :ensure nil
    :demand t
    :config
    (let ((cmd
           (cond
            (sys/linux-x-p
             "xdg-open")
            (t
             ""))))
      (setq
       dired-guess-shell-alist-user
       `(("\\.pdf\\'" ,cmd)
         ("\\.docx\\'" ,cmd)
         ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
         ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
         ("\\.\\(?:xcf\\)\\'" ,cmd)
         ("\\.csv\\'" ,cmd)
         ("\\.tex\\'" ,cmd)
         ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'"
          ,cmd)
         ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
         ("\\.html?\\'" ,cmd)
         ("\\.md\\'" ,cmd))))

    (setq
     dired-omit-files
     (concat
      dired-omit-files
      "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))))

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))


(provide 'lolo-dired)
;;; lolo-dired.el ends here
