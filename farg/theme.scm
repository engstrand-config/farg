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
            farg-theme-accent
            farg-theme-complementary
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
  (accent
   (string "")
   "Accent color.")
  (complementary
   (string "")
   "Complementary accent color.")
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
