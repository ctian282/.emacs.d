(use-package auctex
  :hook (LaTeX-mode . (lambda () (flyspell-mode t)))
  :config
  ;;(use-package cdlatex
  ;;:hook (LaTeX-mode-hook . (lambda () (cdlatex-mode t))))
  )
(use-package cdlatex
  :hook (LaTeX-mode . (lambda () (cdlatex-mode t))))
(provide 'init-latex)
