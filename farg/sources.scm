(define-module (farg sources)
  #:use-module (gnu services configuration)
  #:export (
            farg-source
            <farg-source>
            farg-source?
            farg-source-theme
            farg-source-files
            farg-source-packages
            farg-source-env-vars))

(define-configuration
  farg-source
  (theme
   (farg-theme)
   "A generated farg theme.")
  (files
   (list '())
   "Additional files to install to the users home directory.")
  (packages
   (list '())
   "Additional packages to install to the users profile.")
  (env-vars
   (list '())
   "Environment variables to set when applying the generated theme.")
  (no-serialization))
