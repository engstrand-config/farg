(define-module (farg actions generate)
               #:use-module (guix gexp)
               #:use-module (guix build utils)
               #:use-module (srfi srfi-98) ; for get-environment-variable
               #:use-module (ice-9 getopt-long))

(define-public (start-generator options)
               (begin
                 (let ((%image (option-ref options 'image #f))
                       (%xdg-config-dir (get-environment-variable "XDG_CONFIG_HOME")))
                   (system* "wal" "-i" %image)
                   (install-file
                     %image
                     (if (string? %xdg-config-dir)
                         %xdg-config-dir
                         "~/.config"))
                   (system* "xwallpaper" "--zoom" %image))))
