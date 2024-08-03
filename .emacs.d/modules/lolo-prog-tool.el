;; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package tree-sitter)
(use-package treesit
  :defer t
  :straight (:type built-in)
  :hook ((bash-ts-mode
          c-ts-mode
          c++-ts-mode
          html-ts-mode
          js-ts-mode
          typescript-ts-mode
          json-ts-mode
          rust-ts-mode
          tsx-ts-mode
          python-ts-mode
          css-ts-mode
          go-ts-mode
          yaml-ts-mode) . lsp-deferred)
  :init
  (setq treesit-language-source-alist
        '((astro "https://github.com/virchau13/tree-sitter-astro")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (js ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/Azganoth/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (r "https://github.com/r-lib/tree-sitter-r")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  ;; install all languages above (just run one time, remove after that)
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
  ;; remap current mode with treesitter mode
  (setq major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (html-mode . html-ts-mode)
     (js2-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (css-mode . css-ts-mode)
     (go-mode . go-ts-mode)
     (yaml-mode . yaml-ts-mode))))

;; ============================================================================
(use-package emacsql-psql
  :defer t
  :after (emacsql)
  :straight (:build t))

;; ============================================================================
(use-package
 flycheck
 :straight (:build t)
 :defer t
 :diminish
 :hook (after-init . global-flycheck-mode)
 :commands (flycheck-add-mode)
 :custom
 (flycheck-global-modes
  '(not outline-mode diff-mode shell-mode eshell-mode term-mode))
 (flycheck-emacs-lisp-load-path 'inherit)
 (flycheck-indication-mode
  (if (display-graphic-p)
      'right-fringe
    'right-margin))
 :init
 (if (display-graphic-p)
     (use-package
      flycheck-posframe
      :straight (:build t)
      :custom-face
      (flycheck-posframe-face
       ((t (:foreground ,(face-foreground 'success)))))
      (flycheck-posframe-info-face
       ((t (:foreground ,(face-foreground 'success)))))
      :hook (flycheck-mode . flycheck-posframe-mode)
      :custom
      (flycheck-posframe-position 'window-bottom-left-corner)
      (flycheck-posframe-border-width 3)
      (flycheck-posframe-inhibit-functions
       '((lambda (&rest _) (bound-and-true-p company-backend)))))
   (use-package
    flycheck-pos-tip
    :straight (:build t)
    :defines flycheck-pos-tip-timeout
    :hook (flycheck-mode . flycheck-pos-tip-mode)
    :custom (flycheck-pos-tip-timeout 30)))
 :config
 (use-package
  flycheck-popup-tip
  :straight (:build t)
  :hook (flycheck-mode . flycheck-popup-tip-mode))
 (when (fboundp 'define-fringe-bitmap)
   (define-fringe-bitmap
     'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16]
     nil nil 'center))
 (when (executable-find "vale")
   (use-package
    flycheck-vale
    :straight (:build t)
    :config
    (flycheck-vale-setup)
    (flycheck-add-mode 'vale 'latex-mode))))

;; ============================================================================
(use-package ispell
  :if (executable-find "aspell")
  :defer t
  :straight (:type built-in)
  :config
  (add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
  (add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
  (setq ispell-program-name "aspell"
        ispell-extra-args   '("--sug-mode=ultra" "--run-together")
        ispell-aspell-dict-dir (ispell-get-aspell-config-value "dict-dir")
        ispell-aspell-data-dir (ispell-get-aspell-config-value "data-dir")
        ispell-personal-dictionary (expand-file-name (concat "ispell/" ispell-dictionary ".pws")
                                                     user-emacs-directory)))

(use-package flyspell
  :defer t
  :straight (:type built-in)
  :hook 'org-mode 'markdown-mode 'TeX-mode
  :init
  (defhydra flyspell-hydra ()
    "
Spell Commands^^           Add To Dictionary^^              Other
--------------^^---------- -----------------^^------------- -----^^---------------------------
[_b_] check whole buffer   [_B_] add word to dict (buffer)  [_t_] toggle spell check
[_r_] check region         [_G_] add word to dict (global)  [_q_] exit
[_d_] change dictionary    [_S_] add word to dict (session) [_Q_] exit and disable spell check
[_n_] next error
[_c_] correct before point
[_s_] correct at point
"
    ("B" nil)
    ("b" flyspell-buffer)
    ("r" flyspell-region)
    ("d" ispell-change-dictionary)
    ("G" nil)
    ("n" flyspell-goto-next-error)
    ("c" flyspell-correct-wrapper)
    ("Q" flyspell-mode :exit t)
    ("q" nil :exit t)
    ("S" nil)
    ("s" flyspell-correct-at-point)
    ("t" nil))
  :config
  (provide 'ispell) ;; force loading ispell
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package flyspell-correct
  :defer t
  :straight (:build t)
  :config
  (require 'flyspell-correct-ivy nil t))

(use-package flyspell-correct-ivy
  :defer t
  :straight (:build t)
  :after flyspell-correct)

(use-package flyspell-lazy
  :defer t
  :straight (:build t)
  :after flyspell
  :config
  (setq flyspell-lazy-idle-seconds 1
        flyspell-lazy-window-idle-seconds 3)
  (flyspell-lazy-mode +1))

;; ============================================================================
(use-package lsp-mode
  :defer t
  :straight (:build t)
  :init
  (setq lsp-keymap-prefix "C-c l"
        read-process-output-max (* 3 1024 1024))
  :hook ((c-mode          . lsp-deferred)
         (c++-mode        . lsp-deferred)
         (html-mode       . lsp-deferred)
         (sh-mode         . lsp-deferred)
         (lsp-mode        . lsp-enable-which-key-integration)
         (lsp-mode        . lsp-ui-mode))
  :commands (lsp lsp-deferred)
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.2)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-use-plist t)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "shellcheck")
                    :major-modes '(sh-mode)
                    :remote? t
                    :server-id 'shellcheck-remote)))

(use-package lsp-ui
  :after lsp
  :defer t
  :straight (:build t)
  :commands lsp-ui-mode
  :config
  (require 'lsp-ui-flycheck)
  (require 'lsp-ui-sideline)
  (setq lsp-ui-peek-always-show t
        lsp-ui-doc-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-action t))

(use-package lsp-ivy
  :straight (:build t)
  :defer t
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :defer t
  :straight (:build t))

(use-package consult-lsp
  :defer t
  :after lsp
  :straight (:build t))

(use-package dap-mode
  :after lsp
  :defer t
  :straight (:build t)
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra)))
  :init
  ;; JS/TS
  (with-eval-after-load 'web-mode
    (require 'dap-firefox)
    (require 'dap-chrome)
    (require 'dap-node))

  ;; Rust
  (with-eval-after-load 'rustic-mode
    (require 'dap-lldb)
    (require 'dap-gdb-lldb)
    (dap-register-debug-template
     "Rust::LLDB Run Configuration"
     (list :type "lldb"
           :request "launch"
           :name "LLDB::Run"
           :gdbpath "rust-lldb"
           :target nil
           :cwd nil))))

(use-package langtool
  :defer t
  :straight (:build t)
  :commands (langtool-check
             langtool-check-done
             langtool-show-message-at-point
             langtool-correct-buffer)
  :custom
  (langtool-default-language "en-US")
  (langtool-mother-tongue "fr")
  :config
  (setq langtool-java-classpath (string-join '("/usr/share/languagetool"
                                               "/usr/share/java/languagetool/*")
                                             ":")))

(use-package writegood-mode
  :defer t
  :straight (:build t)
  :hook org-mode latex-mode)

(provide 'lolo-prog-tool)
;;; lolo-prog-tool.el ends here
