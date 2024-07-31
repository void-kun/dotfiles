;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
;; Frame title.
(setq frame-title-format
      '("" "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)
                         " ◉ %s"
                       "  ●  %s - Emacs")
                     project-name))))))

(provide 'lolo-config-with-packages)
;;; lolo-config-with-packages.el ends here
