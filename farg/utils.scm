(define-module (farg utils)
  #:use-module (ice-9 exceptions)
  #:use-module (guix gexp)
  #:export (throw-error file-like-or-path?))

(define* (throw-error msg)
  "Raises a custom exception with message MSG."
  (raise-exception
   (make-exception-with-message
    (string-append "farg: " msg))))

(define (path? x)
  (string? x))

(define (file-like-or-path? x)
  (or (file-like? x) (path? x)))
