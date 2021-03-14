(use-package org-ref
  :config
  )

(setq reftex-default-bibliography '("~/Dropbox/bib/universal.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bib/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bib/universal.bib")
      org-ref-pdf-directory "~/Desktop/bib-lib/bibtex-pdfs/")

(provide 'init-org-ref)
