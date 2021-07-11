(hall-description
  (name "farg")
  (prefix "")
  (version "0.1")
  (author "Fredrik Engstrand")
  (copyright (2021))
  (synopsis
    "farg is a simple system colorscheme generator")
  (description
    "farg is a simple system colorscheme generator with sensible defaults")
  (home-page
    "https://github.com/engstrand-config/farg")
  (license gpl3+)
  (dependencies `(("python-pywal" ,python-pywal)))
  (files (libraries
           ((scheme-file "farg")
            (directory
              "farg"
              ((directory "actions" ((scheme-file "generate")))))))
         (tests ((directory "tests" ())))
         (programs ((directory "scripts" ())))
         (documentation
           ((org-file "README")
            (symlink "README" "README.org")
            (text-file "HACKING")
            (text-file "COPYING")
            (directory "doc" ((texi-file "farg")))))
         (infrastructure
           ((scheme-file "guix") (scheme-file "hall")))))
