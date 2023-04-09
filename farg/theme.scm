(define-module (farg theme)
  #:use-module (gnu services configuration)
  #:use-module (farg utils)
  #:export (
            farg-theme
            <farg-theme>
            farg-theme?
            farg-theme-fg
            farg-theme-bg
            farg-theme-bg-alt
            farg-theme-accent-0
            farg-theme-accent-1
            farg-theme-accent-2
            farg-theme-accent-3
            farg-theme-accent-4
            farg-theme-accent-5
            farg-theme-accent-6
            farg-theme-accent-7
            farg-theme-alpha
            farg-theme-light?
            farg-theme-wallpaper
            farg-theme-other))

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
  (accent-0
   (string "")
   "Accent 0 color.")
  (accent-1
   (string "")
   "Accent 1 color.")
  (accent-2
   (string "")
   "Accent 2 color.")
  (accent-3
   (string "")
   "Accent 3 color.")
  (accent-4
   (string "")
   "Accent 4 color.")
  (accent-5
   (string "")
   "Accent 5 color.")
  (accent-6
   (string "")
   "Accent 6 color.")
  (accent-7
   (string "")
   "Accent 7 color.")
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
