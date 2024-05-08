;;; lolo-python.el --- Zrik's Emacs setup.  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package
 python
 :ensure nil
 :hook
 (inferior-python-mode
  . (lambda () (process-query-on-exit-flag (get-process "Python"))))
 :init
 ;; Disable readline based native completion
 (setq python-shell-completion-native-enable nil)
 :config
 ;; Default to Python 3. Prefer the versioned Python binaries since some
 ;; systems stupidly make the unversioned one point at Python 2.
 (when (and (executable-find "python")
            (string= python-shell-interpreter "python"))
   (setq python-shell-interpreter "python"))

 ;; Env vars
 (with-eval-after-load 'exec-path-from-shell
   (exec-path-from-shell-copy-env "PYTHONPATH")))

(provide 'lolo-python)
;;; lolo-python.el ends here
