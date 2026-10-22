(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("04aa1c3ccaee1cc2b93b246c6fbcd597f7e6832a97aaeac7e5891e6863236f9f" default))
 '(safe-local-variable-values
   '((haskell-process-type . cabal-repl)
     (eval push 'haskell-stack-ghc flycheck-disabled-checkers)
     (haskell-hoogle-url . "http://localhost:8123/?hoogle=%s")
     (ormolu-process-path . "fourmolu")
     (lsp-haskell-formatting-provider . "fourmolu") (haskell-stylish-on-save)
     (haskell-process-type . stack-ghci)
     (haskell-indentation-starter-offset . 4)
     (haskell-indentation-left-offset . 4)
     (haskell-indentation-layout-offset . 4)
     (sql-postgres-login-params (user :default "app")
      (server :default "localhost") (database :default "main"))
     (sql-postgres-options "-Uapp" "-P" "pager=off") (sql-server . "localhost")
     (sql-database . "main") (web-mode-code-indent-offset . 2)
     (web-mode-css-indent-offset . 2) (web-mode-markup-indent-offset . 2))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'projectile-ripgrep 'disabled nil)
