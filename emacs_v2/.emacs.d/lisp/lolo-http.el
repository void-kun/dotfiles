;;; lolo-http.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; 
;;
;;; Code:

(maybe-require-package 'httprepl)
(with-eval-after-load 'httprepl
  (push '("image" . image) httprepl-content-type-alist)
  (push '(image . ((lambda (b) (with-current-buffer b
                                 (image-mode)
                                 b))))
        httprepl-content-type-middleware-alist))


(when (maybe-require-package 'restclient)
  (add-auto-mode 'restclient-mode "\\.rest\\'")

  (defun sanityinc/restclient ()
    "Work with `rest' in the *restclient* buffer."
    (interactive)
    (with-current-buffer (get-buffer-create "*restclient*")
      (restclient-mode)
      (pop-to-buffer (current-buffer)))))

(provide 'lolo-http)
;;; lolo-http.el ends here
