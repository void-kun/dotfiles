;;; lolo-docker.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(when (maybe-require-package 'docker)
  (sanityinc/fullframe-mode 'docker-image-mode)
  (sanityinc/fullframe-mode 'docker-machine-mode)
  (sanityinc/fullframe-mode 'docker-volume-mode)
  (sanityinc/fullframe-mode 'docker-network-mode)
  (sanityinc/fullframe-mode 'docker-container-mode))
(maybe-require-package 'dockerfile-mode)
(maybe-require-package 'docker-compose-mode)

(provide 'lolo-docker)
;;; lolo-docker.el ends here
