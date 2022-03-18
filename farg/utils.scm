(define-module (farg utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define-public (list-of-services? lst)
  (every service? lst))

(define-public (maybe-string? str)
  (or (string? str) (eq? str #f)))

;; Taken from https://github.com/abcdw/rde/blob/ \
;; 5f525af67d4528ffbf4ac8e0ade5ed8b46347643/gnu/home-services-utils.scm#L90-L99
(define* (slurp-file-gexp file #:key (encoding "UTF-8"))
  "Returns a gexp, which reads all the content of the FILE and returns
it as a string.  FILE must be a file-like object."
  (when (not (file-like? file))
    (raise (formatted-message
            (G_ "~a is not a file-like object.")
            file)))
  #~(call-with-input-file #$file
      (@ (ice-9 textual-ports) get-string-all)
      #:encoding #$encoding))
