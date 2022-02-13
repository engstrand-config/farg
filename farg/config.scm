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
            farg-config-palette-getter
            farg-config-colors-directory
            farg-config-wallpaper-path
            farg-config-activation-commands
            farg-config-color-files))

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
  (colors-directory
   (maybe-string (string-append (or (getenv "XDG_CACHE_HOME")
                                    (string-append (getenv "HOME") "/.cache"))
                                "/wal"))
   "Directory to save the serialized contents of @code{colors} to. Can be used to
provide external applications with the generated colorscheme, e.g. pywalfox.")
  (wallpaper-path
   (maybe-string (or (getenv "XDG_CONFIG_HOME")
                     (string-append (getenv "HOME") "/.config")))
   "Path to save the current wallpaper set by @code{wallpaper}.")
  (activation-commands
   (list '())
   "List of commands to run when the new home environment has been activated.
This can be used to update currently running applications, e.g. pywalfox.")
  (color-files
   (list-of-strings '("colors"))
   "List of color files to export to @code{colors-directory}.
By default, only the @file{colors} file will be exported. To export other files,
specify the file name as a string. Non-existing files will be ignored.
@example
'(\"colors\" \"colors.json\" \"colors.css\")
@end example")
  (no-serialization))
