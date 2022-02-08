(define-module (farg config)
  #:use-module (farg utils)
  #:use-module (gnu services configuration)
  #:export (
            farg-config
            <farg-config>
            farg-config?
            farg-config-backend
            farg-config-saturation
            farg-config-wallpaper
            farg-config-light?))

(define-configuration
  farg-config
  (light?
   (boolean #f)
   "If a light colorscheme should be generated.")
  (wallpaper
   (maybe-string #f)
   "Path for wallpaper to use in colorscheme generation.")
  (saturation
   (number 1.0)
   "Colorscheme saturation.")
  (backend
   (string "wal")
   "Colorscheme generator backend to use.")
  (no-serialization))
