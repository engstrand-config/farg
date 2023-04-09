(define-module (farg theme)
  #:use-module (ice-9 match)
  #:use-module (gnu services configuration)
  #:use-module (farg utils)
  #:export (
            farg-theme
            <farg-theme>
            farg-theme?
            farg-theme-fg
            farg-theme-bg
            farg-theme-bg-alt
            farg-theme-accent
            farg-theme-alpha
            farg-theme-light?
            farg-theme-wallpaper
            farg-theme-other

            farg:get-attr))

(define-configuration
  farg-theme
  (fg
   (string "")
   "Main text color.")
  (bg
   (string "")
   "Main background color.")
  (bg-alt
   (string "")
   "Alternative background color.")
  (accent
   (string "")
   "Accent color.")
  (alpha
   (number 1.0)
   "Default transparency for the theme (0-1).")
  (light?
   (boolean #f)
   "If it is a light theme")
  (wallpaper
   (maybe-string #f)
   "The wallpaper used in this theme, if any.")
  (other
   (list '())
   "Alist of custom colors/attributes. These keys can easily
be fetched using the farg theme helper utils.")
  (no-serialization))

(define* (farg:get-attr theme field)
  "Quickly get the value of a field FIELD in your farg theme."
  (match field
    ('fg (farg-theme-fg theme))
    ('bg (farg-theme-bg theme))
    ('bg-alt (farg-theme-bg-alt theme))
    ('accent (farg-theme-accent theme))
    ('alpha (farg-theme-alpha theme))
    ('light? (farg-theme-light? theme))
    ('wallpaper (farg-theme-wallpaper theme))
    (_
     (let ((value (assoc-ref (farg-theme-other theme) field)))
       (if value
           value
           (throw-error
            (format #f "invalid attribute '~a' not found in theme" field)))))))
