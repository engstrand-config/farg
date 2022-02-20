(define-module (farg picker)
  #:use-module (guix gexp)
  #:use-module (ice-9 rdelim)
  #:use-module (farg config)
  #:export (sxiv-wallpaper-picker))

(define* (sxiv-wallpaper-picker config)
  "Default wallpaper picker using sxiv."
  (let* ((path (farg-config-wallpaper-search-directory config))
         (port (open-input-pipe (string-append "sxiv -t -o -i " path)))
         (chosen-wallpaper (read-line port)))
    (close-pipe port)
    (if chosen-wallpaper
        (display (format #f "Using wallpaper: '~a'..." chosen-wallpaper))
        (display "No wallpaper chosen, skipping..."))
    chosen-wallpaper))
