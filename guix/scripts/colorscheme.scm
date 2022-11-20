(define-module (guix scripts colorscheme)
  #:use-module (guix ui)
  #:use-module (guix scripts)
  #:use-module (guix combinators)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix read-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module (farg utils)
  #:export (guix-colorscheme))

(define %default-options
  '(()))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\L "load-path") #t #f
                (lambda (opt name arg result)
                  ;; XXX: Imperatively modify the search paths.
                  (set! %load-path (cons arg %load-path))
                  (set! %load-compiled-path (cons arg %load-compiled-path))
                  result))))

(define (show-help)
  (display (G_ "Usage: guix colorscheme [OPTIONS...] ACTION
Manage your system colorscheme with farg according to ACTION.\n"))
  (newline)
  (display (G_ "The valid values for ACTION are:\n"))
  (newline)
  (display (G_ "\
   pick   select a new wallpaper using the wallpaper picker\n"))
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (display (G_ "
  -h, --help             display this help and exit")))

(define-command (guix-colorscheme . args)
  (category extension)
  (synopsis "update your system colorscheme")

  (define (handle-argument arg result arg-handler)
    ;; Treat all non-option arguments as package specs.
    (values (alist-cons 'action (string->symbol arg) result)
            arg-handler))

  (define opts
    (parse-command-line args %options (list %default-options #f)
                        #:argument-handler handle-argument))
  (with-error-handling
    (let* ((command (assoc-ref opts 'action)))
      (if command
        (case command
          ((pick) (wallpaper-picker))
          ((light) (update-colorscheme-mode command #t))
          ((dark) (update-colorscheme-mode command #f))
          (else (leave (G_ "~a: unknown action~%") command)))
        (show-help)))))

;;;
;;; Actions
;;;

(define* (wallpaper-picker)
  (display (G_ "picker\n")))

(define* (update-colorscheme-mode command light)
  (let ((current-mode (getenv "GUIX_FARG_LIGHT")))
    (if (not (equal? (serialize-boolean light) current-mode))
        (if light
            (display (G_ "enable light mode\n"))
            (display (G_ "enable dark mode\n")))
        (info (G_ "~a: mode already activated~%") command))))
