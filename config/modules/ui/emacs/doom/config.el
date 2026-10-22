;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'sanityinc-tomorrow-eighties)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Org/")

(after! org
  (setq org-startup-folded t))

;(package! ormolu)

(after! haskell
  (setq!
   haskell-stylish-on-save nil
   lsp-haskell-plugin-stan-global-on nil
   lsp-haskell-check-project nil
   lsp-haskell-formatting-provider "fourmolu")

  (setq-hook! 'haskell-mode-hook
    compile-command haskell-compile-stack-build-command
    ormolu-process-path "fourmolu")

  (add-hook! 'haskell-mode-hook
    (defun +haskell-disable-flycheck-ghc-h ()
      "Always use HLS via lsp; suppress flycheck's haskell-ghc/stack-ghc fallbacks."
      (dolist (checker '(haskell-ghc haskell-stack-ghc))
        (add-to-list 'flycheck-disabled-checkers checker))))

  ;(add-hook! haskell-mode 'ormolu-format-on-save-mode)
  )


(after! lsp-haskell
  (setq! lsp-haskell-plugin-ormolu-config-external "fourmolu")
  (setq! lsp-haskell-plugin-stan-global-on nil))

(after! lsp-mode
  (setq! lsp-ui-sideline-enable nil))

(after! org-roam
  (setq! org-roam-directory "~/Org/org-roam/"))

;; Liquidsoap syntax highlighting. The keywords come from the upstream
;; scripts/liquidsoap-mode.el in savonet/liquidsoap. The upstream file needs
;; a build-generated completions file and company, so we do not load it.
(defvar liquidsoap-font-lock-keywords
  `((,(concat "^[ \t]*"
              (regexp-opt '("%ifdef" "%ifndef" "%ifencoder" "%ifnencoder"
                            "%ifversion" "%else" "%endif" "%include")))
     . font-lock-preprocessor-face)
    (,(regexp-opt '("fun" "def" "rec" "replaces" "eval" "begin" "end" "if"
                    "then" "else" "elsif" "let" "try" "catch" "while" "for"
                    "in" "to" "do" "open")
                  'words)
     . font-lock-keyword-face)
    ("->" . font-lock-keyword-face)
    (,(regexp-opt '("and" "or" "not" "mod") 'words) . font-lock-builtin-face)
    ("??\\|:=" . font-lock-builtin-face)
    (,(regexp-opt '("true" "false" "null") 'words) . font-lock-constant-face)
    ("\\_<def[ \t]+\\(?:rec[ \t]+\\|replaces[ \t]+\\)?\\([^ \t(=]+\\)"
     1 font-lock-function-name-face)))

(defvar liquidsoap-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `liquidsoap-mode'.")

(define-derived-mode liquidsoap-mode prog-mode "Liquidsoap"
  "Major mode for Liquidsoap scripts."
  (setq-local font-lock-defaults '(liquidsoap-font-lock-keywords))
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local tab-width 2))

(add-to-list 'auto-mode-alist '("\\.liq\\'" . liquidsoap-mode))

;; (setq +format-on-save-enabled-modes
;;       '(not emacs-lisp-mode    ; elisp's mechanisms are good enough
;;         not sql-mode           ; sqlformat is currently broken
;;         tex-mode           ; latexindent is broken
;;         latex-mode
;;         haskell-mode
;;         markdown-mode
;;         json-mode
;;         ) ; doesn't need a formatter
;;       )

(map! :leader
      :prefix "w"
      :desc "Delete other windows" "o" #'delete-other-windows)

(map! :leader
      :prefix "e"
      :desc "Flycheck Next Error" "k" #'flycheck-next-error)

(map! :leader
      :prefix "e"
      :desc "Flycheck Next Error" "j" #'flycheck-previous-error)

(map! :leader
      :prefix "g"
      :desc "github link" "L" #'git-link)

(map! :leader
      :desc "Deadgrep project"
      "s p" #'deadgrep)

(setq org-ditaa-jar-path "@ditaa@/lib/ditaa.jar")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-hook 'before-save-hook 'delete-trailing-whitespace) ; using ws-butler for now

(setq kill-do-not-save-duplicates nil)
