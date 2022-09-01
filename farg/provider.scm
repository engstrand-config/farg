(define-module (farg provider)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (farg config)
  #:use-module (farg colorscheme)
  #:use-module (farg home-service)
  #:use-module (gnu services configuration)
  #:export (
            colorscheme-provider
            make-colorscheme-accessor))

(define* (make-colorscheme-accessor colorscheme)
  "Creates a helper procedure for quickly accessing base and palette
colors from a generated colorscheme.
@example
(define palette (make-colorscheme-accessor colorscheme))
(palette 'background) ;; returns the color in background field of colorscheme
(palette 'my-custom-palette-color) ;; returns color from palette alist
@end example"
  (lambda (name)
    (match name
      ('primary (colorscheme-primary colorscheme))
      ('secondary (colorscheme-secondary colorscheme))
      ('text (colorscheme-text colorscheme))
      ('primary-text (colorscheme-primary-text colorscheme))
      ('secondary-text (colorscheme-secondary-text colorscheme))
      ('background (colorscheme-background colorscheme))
      (_ (let ((color (assoc-ref (colorscheme-palette colorscheme) name)))
           (if color
               color
               (raise-exception
                (make-exception-with-message
                 (string-append "farg: '"
                                (symbol->string name)
                                "' does not exist in your colorscheme.")))))))))

(define* (should-generate-colorscheme? config)
  "Checks if a new colorscheme should be generated with pywal."
  (let ((saved-wallpaper (getenv "GUIX_FARG_WALLPAPER"))
        (saved-backend (getenv "GUIX_FARG_BACKEND"))
        (saved-saturation (getenv "GUIX_FARG_SATURATION"))
        (saved-light? (getenv "GUIX_FARG_LIGHT")))
    (cond
     ((not (eq? (farg-config-light? config) saved-light?)) #t)
     ((not (eq? (farg-config-backend config) saved-backend)) #t)
     ((not (eq? (farg-config-saturation config) saved-saturation)) #t)
     ((not (eq? (farg-config-wallpaper config) saved-wallpaper)) #t)
     (else #f))))

(define* (colorscheme-provider
          #:key
          (config (farg-config))
          (services '()))
  "Provides a generated colorscheme to each service in SERVICES."
  (define new-colorscheme
    (colors->colorscheme
     (if (should-generate-colorscheme? config)
      ;; TODO: Should the temporary path be a configuration option?
      (generate-colorscheme config (farg-config-temporary-directory config))
      (read-colorscheme (farg-config-colors-directory config)))
     config))

  (define palette (make-colorscheme-accessor new-colorscheme))
  (define home-config
    (home-farg-configuration
     (config config)
     (colorscheme new-colorscheme)))

  (cond
   ((list? services)
    (map (lambda (service)
           (let ((arity (procedure-minimum-arity service)))
             (if (or (eq? arity #f) (not (eq? (car arity) 2)))
                 service
                 (service home-config palette))))
         services))
   ((procedure? services) (services home-config palette))
   (else services)))
