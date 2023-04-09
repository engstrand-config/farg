(define-module (farg packages)
  #:use-module (guix packages)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages python-xyz))

;; HACK: Upstreamed pywal does not propagate imagemagick which
;; means that it is not available during runtime.
(define-public python-pywal-farg
  (package
   (inherit python-pywal)
   (name "python-pywal-farg")
   (inputs '())
   (propagated-inputs (list imagemagick))))
