(define-module (farg provider)
  #:use-module (srfi srfi-1)
  #:use-module (farg config)
  #:use-module (farg colorscheme)
  #:use-module (gnu services configuration)
  #:export (
            themed-services
            <themed-services>
            themed-services?
            themed-services-home
            themed-services-system
            colorscheme-provider))

(define-configuration
  themed-services
  (home
   (list '())
   "A list of themed home services.")
  (system
   (list '())
   "A list of themed system services.")
  (no-serialization))

(define* (colorscheme-provider
          #:key
          (config (farg-config))
          (home-services '())
          (system-services '()))
  "Provides a generated colorscheme to each service generator."

  ;; Side-effect. Call pywal and generate colors.
  (generate-colorscheme config)

  ;; Convert generated colors into a colorscheme record.
  (define new-colorscheme
    (colors->colorscheme (read-generated-colorscheme) config))

  (define (generate-services provider new-colorscheme)
    (cond
     ((list? provider)
      (if (every (lambda (x) (>= 1 (car (procedure-minimum-arity x))))
                 provider)
          (map (lambda (s) (s new-colorscheme)) provider)
          provider))
     ((procedure? provider) (provider new-colorscheme))
     (else provider)))

  (themed-services
     (home (generate-services home-services new-colorscheme))
     (system (generate-services system-services new-colorscheme)))))
