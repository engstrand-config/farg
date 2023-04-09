(define-module (farg provider)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services configuration)
  #:use-module (farg utils)
  #:use-module (farg theme)
  #:use-module (farg sources)
  #:export (farg:theme-provider))

(define* (farg:theme-provider source-generator services
                              #:key (palette-extension #f))
  "Provides SOURCE and its generated theme to each service in SERVICES.
Specifying a procedure to PALETTE-EXTENSION allows you to wrap the default
palette and add your own custom colors."
  (define source
    (if (procedure? source-generator)
        (source-generator)
        source-generator))

  (define default-palette
    (let ((theme (farg-source-theme source)))
      (lambda (field)
        (match field
          ('fg (farg-theme-fg theme))
          ('bg (farg-theme-bg theme))
          ('bg-alt (farg-theme-bg-alt theme))
          ('accent (farg-theme-accent theme))
          ('complementary (farg-theme-complementary theme))
          ('alpha (farg-theme-alpha theme))
          ('light? (farg-theme-light? theme))
          ('wallpaper (farg-theme-wallpaper theme))
          (_
           (let ((value (assoc-ref (farg-theme-other theme) field)))
             (if value
                 value
                 (throw-error
                  (format #f "invalid attribute '~a' not found in theme" field)))))))))

  (define palette
    (if (procedure? palette-extension)
        (palette-extension default-palette)
        default-palette))

  (cond
   ((list? services)
    (map (lambda (service)
           (let ((arity (procedure-minimum-arity service)))
             (if (or (eq? arity #f) (not (eq? (car arity) 2)))
                 service
                 (service source palette))))
         services))
   ((procedure? services) (services source palette))
   (else services)))
