(define-module (farg actions generate)
               #:use-module (ice-9 getopt-long))

(define-public (start-generator options)
               (begin
                 (display (option-ref options 'image #f))))
