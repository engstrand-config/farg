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

(define* (colorscheme-provider
          #:key
          (config (farg-config))
          (services '()))
  "Provides a generated colorscheme to each service in SERVICES."

  (define wallpaper
    (let ((getter (farg-config-wallpaper config)))
      (if (procedure? getter)
          (getter (farg-config-light? config))
          getter)))

  (define colors
    (let* ((colors-dir (farg-config-colors-directory config))
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
          (generate-colorscheme wallpaper config (farg-config-temporary-directory config)))))

  (define new-colorscheme
    (colors->colorscheme wallpaper colors config))

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
