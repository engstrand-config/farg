(define-module (farg picker)
  #:use-module (guix gexp)
  #:use-module (gnu packages image-viewers)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (farg config)
  #:export (sxiv-wallpaper-picker))

(define* (sxiv-wallpaper-picker search-path)
  "Default wallpaper picker using sxiv."

  (define exec-command
    (list (file-append sxiv "/bin/sxiv")
          "-t"
          "-o"
          "-i"
          "-r"
          search-path))

  #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 rdelim)
                   (ice-9 match)
                   (srfi srfi-1))

      (call-with-values
          (lambda ()
            (pipeline `(#$exec-command)))
        (lambda (from to pids)
          (close-port to)
          (let ((chosen-wallpaper (read-line from)))
            (close-port from)
            (match-let* (((pid) pids)
                         ((_ . status) (waitpid pid)))
              (when (zero? (status:exit-val status))
                (unless (eof-object? chosen-wallpaper)
                    (display chosen-wallpaper)))))))))
