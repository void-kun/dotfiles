;;; -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;
;;; Code:

;; ============================================================================
(use-package treesit
  :defer t
  :straight (:type built-in)
  :hook ((bash-ts-mode c-ts-mode c++-ts-mode
          html-ts-mode js-ts-mode typescript-ts-mode
          json-ts-mode rust-ts-mode tsx-ts-mode python-ts-mode
          css-ts-mode yaml-ts-mode) . lsp-deferred)
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
          (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))

;; ============================================================================
;; (use-package appwrite
;;   :defer t
;;   :straight (appwrite :build t
;;                       :type git
;;                       :host github
;;                       :repo "Phundrak/appwrite.el")
;;   :config
;;   (csetq appwrite-endpoint "https://appwrite.phundrak.com"
;;          appwrite-devel t))

;; ============================================================================
(use-package emacsql-psql
  :defer t
  :after (emacsql)
  :straight (:build t))

;; ============================================================================
(use-package flycheck
  :straight (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)
  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)
  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.2))

(use-package flycheck-popup-tip
  :straight (:build t)
  :after (flycheck evil)
  :hook (flycheck-mode . flycheck-popup-tip-mode)
  :config
  (setq flycheck-popup-tip-error-prefix "X ")
  (with-eval-after-load 'evil
    (add-hook 'evil-insert-state-entry-hook
              #'flycheck-popup-tip-delete-popup)
    (add-hook 'evil-replace-state-entry-hook
              #'flycheck-popup-tip-delete-popup)))

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
  :ghook 'org-mode 'markdown-mode 'TeX-mode
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
  :general ([remap ispell-word] #'flyspell-correct-at-point)
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
  (lsp-idle-delay 0.6)
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
  :straight (:build t)
  :general
  (lolo/evil
    :keymaps 'lsp-mode-map
    [remap xref-find-apropos] #'consult-lsp-symbols))

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
