
;;; lolo-docker.el --- xxx.	-*- lexical-binding: t -*-

;; Author: hoangzrik
;; URL: https://github.com/void-kun/dotfiles

;;; Code:

(eval-when-compile
  (require 'lolo-const))

;; Docker
(use-package docker
  :defines docker-image-run-arguments
  :bind ("C-c D" . docker)
  :init (setq docker-image-run-arguments '("-i" "-t" "--rm")
              docker-container-shell-file-name "/bin/bash"))

(use-package dockerfile-mode)

(provide 'lolo-docker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lolo-docker.el ends here
