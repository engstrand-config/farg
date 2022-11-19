(define-module (farg config)
  #:use-module (farg utils)
  #:use-module (farg picker)
  #:use-module (guix packages)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages image-viewers)
  #:export (
            farg-config
            <farg-config>
            farg-config?
            farg-config-fields

            farg-config-light?
            farg-config-backend
            farg-config-wallpaper
            farg-config-alpha
            farg-config-saturation
            farg-config-palette-getter
            farg-config-temporary-directory
            farg-config-colors-directory
            farg-config-wallpaper-path
            farg-config-activation-commands
            farg-config-color-files
            farg-config-wallpaper-search-directory
            farg-config-wallpaper-picker-package
            farg-config-wallpaper-picker))

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
   "Absolute path for wallpaper to use in colorscheme generation.")
  (alpha
   (number-or-getter 1.0)
   "Colorscheme transparency. If a procedure is provided, it will be called
with the value of @code{light?} during colorscheme generation.")
  (saturation
   (number-or-getter 1.0)
   "Colorscheme saturation. If a procedure is provided, it will be called
with the value of @code{light?} during colorscheme generation.")
  (palette-getter
   (maybe-palette-getter #f)
   "Procedure that accepts two arguments, a @code{colorscheme} record and
the generated pywal colors as a list of index/color pairs. The return value
of this procedure will be used to set the @code{(palette)} field of the
final @code{colorscheme} record. This can be used to generate more shades
of the pywal-generated colors.")
  (temporary-directory
   (string "/tmp/farg")
   "Temporary directory for pywal files. All pywal generated files will be placed
here to prevent polluting your home environment. To select files to export to
your home environment, see @code{colors-files}.")
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
   (list-of-strings '("colors" "colors.json"))
   "List of color files to export to @code{colors-directory}.
By default, only the @file{colors} and @file{colors.json} files will be exported.
To export other files, specify the file name as a string. Non-existing files will be ignored.
@example
'(\"colors\" \"colors.json\" \"colors.css\")
@end example")
  (wallpaper-search-directory
   (maybe-string #f)
   "Path to a directory with your wallpapers. This will be used by the wallpaper
picker utility, allowing you to quickly choose a new wallpaper to generate
a colorscheme for.")
  (wallpaper-picker-package
   (package sxiv)
   "Package for the application to use for the wallpaper picker utility.
The chosen package will be installed to your home environment.")
  (wallpaper-picker
   (procedure sxiv-wallpaper-picker)
   "Procedure to call for picking a wallpaper. Should accept the config as
argument, and return the full path to the selected wallpaper.")
  (no-serialization))
