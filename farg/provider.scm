(define-module (farg provider)
               #:use-module (farg colorscheme)
               #:use-module (gnu service configuration)
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
    (list-of-services '())
    "A list of themed home services.")
  (system
    (list-of-services '())
    "A list of themed system services.")
  (no-serialization))

(define* (colorscheme-provider
           #:key
           (config (colorscheme))
           (home-service-generator #f)
           (system-service-generator #f))
         "Provides a generated colorscheme to each service generator."
         (let ((new-colorscheme (generate-colorscheme config)))
           (themed-services
             (home
               (if home-service-generator
                   (home-service-generator new-colorscheme)
                   '()))
             (system
               (if system-service-generator
                   (system-service-generator new-colorscheme)
                   '())))))
