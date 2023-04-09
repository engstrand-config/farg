(define-module (farg utils)
  #:export (farg:throw-error))

(define* (throw-error msg)
  "Raises a custom exception with message MSG."
  (raise-exception
   (make-exception-with-message
    (string-append "farg: '" msg))))
