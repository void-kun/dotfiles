;;; lolo-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(defun pyrightconfig-write (visualenv)
  "Choose the python environment by `VISUALENV'."
  ;; (interactive "VENV Dir: ")
  (interactive "DEnv: ")
  (let* ((venv-dir (tramp-file-local-name (file-truename visualenv)))
	 (venv-file-name (directory-file-name venv-dir))
	 (venvPath (file-name-directory venv-file-name))
	 (venv (file-name-base venv-file-name))

	 (base-dir (vc-git-root default-directory))
	 (out-file (expand-file-name "pyrightconfig.json" base-dir))

	 (out-contents (json-encode (list :venvPath venvPath :venv venv))))
    (with-temp-file out-file (insert out-contents))))

(provide 'lolo-python)
;;; lolo-python.el ends here
