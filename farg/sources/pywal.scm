(define-module (farg sources pywal)
  #:use-module (guix gexp)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (farg utils)
  #:use-module (farg theme)
  #:use-module (farg colors)
  #:use-module (farg sources)
  #:use-module (farg packages)
  #:export (farg:generator-pywal))

(define* (farg:generator-pywal wallpaper
                               #:key
                               (light? #f)
                               (saturation 1.0)
                               (generator #f))
  "Generate a farg theme based on WALLPAPER using pywal.
The pywal theme generation can be modified using the LIGHT,
and SATURATION. An additional GENERATOR procedure can be specified
in order to customize which colors are used, and adjust colors as
needed."
  (lambda ()
    (when (or (not (string? wallpaper))
              (not (file-exists? wallpaper)))
      (throw-error (format #f "invalid wallpaper path: ~a" wallpaper)))

    (define pywal-cache-path "/tmp/pywal")

    (define (run-generator colors)
      (if (procedure? generator)
          (generator colors)
          (farg-theme
           (fg (assoc-ref colors 15))
           (bg (assoc-ref colors 0))
           (bg-alt (farg:offset (assoc-ref colors 0)))
           (accent (assoc-ref colors 10))
           (light? light?)
           (wallpaper wallpaper)
           (other colors))))

    (define (run-pywal wallpaper)
      (let ((home-service-activated? (getenv "GUIX_FARG_WALLPAPER"))
            (is-root? (equal? (geteuid) 0)))
        (begin
          (when (and (not home-service-activated?) (not is-root?))
            (system "guix install python-pywal-farg"))
          (display "Generating farg theme using pywal...\n")
          (system
           (string-join
            (list (string-append "PYWAL_CACHE_DIR=" pywal-cache-path)
                  "$(guix build python-pywal-farg)/bin/wal"
                  "-i" wallpaper
                  "--backend" "wal"
                  "--saturate" (number->string saturation)
                  (if light? "-l" "")
                  "-e" "-t" "-s" "-n"
                  "-q")
            " "))
          ;; Remove again, since it is being added via the home service
          (when (and (not home-service-activated?) (not is-root?))
            (system "guix remove python-pywal-farg")))))

    (define (read-pywal-colors)
      (define (read-colors port acc index)
        (let ((color (read-line port)))
          (if (eof-object? color)
              acc
              (read-colors port
                           (cons `(,index . ,color) acc)
                           (+ index 1)))))
      (read-colors
       (open-input-file (string-append pywal-cache-path "/colors")) '() 0))

    (begin
      (run-pywal wallpaper)
      (farg-source
       (theme (run-generator (read-pywal-colors)))
       (packages (list python-pywal-farg))
       (files
        `(,@(map
             (lambda (out-file)
               `((,(string-append ".cache/wal/" out-file) .
                  ,(local-file (string-append pywal-cache-path "/" out-file)))))
             ;; Only include regular files, no directories
             (filter-map
              (lambda (file)
                (if (equal? (stat:type (stat (string-append pywal-cache-path "/" file)))
                            'regular)
                    file
                    #f))
              (scandir pywal-cache-path)))))))))
