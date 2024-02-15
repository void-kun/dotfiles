;;; lolo-company.el --------------------------------

;;; Code:
(lolo-require-packages '(company))

(require 'company)
(require 'diminish)

(setq company-idle-delay 0)
(setq company-show-quick-access t)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)
(diminish 'company-mode)

(provide 'lolo-company)
;;; lolo-company.el ends here
