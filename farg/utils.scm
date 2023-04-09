(define-module (farg utils)
  #:use-module (ice-9 exceptions)
  #:export (farg:throw-error maybe-string?))

(define* (throw-error msg)
  "Raises a custom exception with message MSG."
  (raise-exception
   (make-exception-with-message
    (string-append "farg: '" msg))))

(define* (maybe-string? str)
  (or (boolean? str) (string? str)))
