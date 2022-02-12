(define-module (farg config)
  #:use-module (farg utils)
  #:use-module (gnu services configuration)
  #:export (
            farg-config
            <farg-config>
            farg-config?
            farg-config-light?
            farg-config-backend
            farg-config-wallpaper
            farg-config-saturation
            farg-config-palette-getter))

(define (maybe-palette-getter? proc)
  (or (eq? proc #f)
      (eq? (car (procedure-minimum-arity proc)) 2)))

(define-configuration
  farg-config
  (light?
   (boolean #f)
   "If a light colorscheme should be generated.")
  (backend
   (string "wal")
   "Colorscheme generator backend to use.")
  (wallpaper
   (maybe-string #f)
   "Path for wallpaper to use in colorscheme generation.")
  (saturation
   (number 1.0)
   "Colorscheme saturation.")
  (palette-getter
   (maybe-palette-getter #f)
   "Procedure that accepts two arguments, a @code{colorscheme} record and
the generated pywal colors as a list of index/color pairs. The return value
of this procedure will be used to set the @code{(palette)} field of the
final @code{colorscheme} record. This can be used to generate more shades
of the pywal-generated colors.")
  (no-serialization))
