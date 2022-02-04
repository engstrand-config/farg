(define-module (farg colorscheme)
               #:use-module (gnu services configuration)
               #:export (
                         colorscheme
                         <colorscheme>
                         colorscheme?
                         colorscheme-wallpaper))

(define-configuration
  colorscheme
  (wallpaper
    (maybe-string #f)
    "Colorscheme wallpaper")
  (no-serialization))
