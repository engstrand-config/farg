(define-module (farg provider)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 exceptions)
  #:use-module (farg utils)
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
      ('alpha (colorscheme-alpha colorscheme))
      ('primary (colorscheme-primary colorscheme))
      ('secondary (colorscheme-secondary colorscheme))
      ('text (colorscheme-text colorscheme))
      ('primary-text (colorscheme-primary-text colorscheme))
      ('secondary-text (colorscheme-secondary-text colorscheme))
      ('background (colorscheme-background colorscheme))
      (_
       (let* ((alist (if (number? name)
                         (colorscheme-raw colorscheme)
                         (colorscheme-palette colorscheme)))
              (color (assoc-ref alist name)))
         (if color
             color
             (raise-exception
              (make-exception-with-message
               (string-append "farg: '"
                              (symbol->string name)
                              "' does not exist in your colorscheme.")))))))))

(define* (colorscheme-provider
          #:key
          (config (farg-config))
          (services '()))
  "Provides a generated colorscheme to each service in SERVICES."

  (define colors
    (let* ((wallpaper (farg-config-wallpaper config))
           (colors-dir (farg-config-colors-directory config))
           (previous-colors (string-append colors-dir "/colors")))
      (if (or (eq? wallpaper #f)
              (not (file-exists? wallpaper)))
          (if (file-exists? previous-colors)
              (read-colorscheme colors-dir)
              ;; TODO: Add option for fallback colorscheme colors
              ;; Fallback to some default colorscheme if none could be found
              '((0 . "#121216")
                (1 . "#1200f6")
                (2 . "#ff967c")
                (3 . "#6cadff")
                (4 . "#80b6ff")
                (5 . "#91c2fe")
                (6 . "#99c4ff")
                (7 . "#dadce0")
                (8 . "#989a9c")
                (9 . "#1200f6")
                (10 . "#ff967c")
                (11 . "#6cadff")
                (12 . "#80b6ff")
                (13 . "#91c2fe")
                (14 . "#99c4ff")
                (15 . "#dadce0")))
          (generate-colorscheme config (farg-config-temporary-directory config)))))

  (define new-colorscheme
    (colors->colorscheme colors config))

  (define palette
    (let ((custom-getter (farg-config-palette-getter config))
          (default-palette (make-colorscheme-accessor new-colorscheme)))
      (if (procedure? custom-getter)
          (custom-getter default-palette)
          default-palette)))

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
