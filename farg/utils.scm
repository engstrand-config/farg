(define-module (farg utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define-public (list-of-services? lst)
  (every service? lst))

(define-public (maybe-string? str)
  (or (string? str) (eq? str #f)))
