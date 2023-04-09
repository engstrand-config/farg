(define-module (farg reload)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (reload-terminal-colors))

;; Procedures adapted from pywal, see:
;; https://github.com/dylanaraps/pywal/blob/master/pywal/sequences.py
(define (set-special index color)
  (format #f "\x1b]~d;~a\x1b\\" index color))

(define (set-color index color)
  (format #f "\x1b]4;~d;~a\x1b\\" index color))

(define (get-color key palette overrides)
  (let ((value (assoc-ref overrides key)))
    (if (eq? value #f)
        (palette key)
        value)))

(define (create-sequences palette overrides)
  (let ((text (get-color 'fg palette overrides))
        (background (get-color 'bg palette overrides)))
    (string-join
     (append (map (lambda (x) (set-color x (get-color x palette overrides)))
                  (iota 16 0))
             (list (set-special 10 text)
                   (set-special 11 background)
                   (set-special 12 text)
                   (set-special 13 text)
                   (set-special 17 text)
                   (set-special 19 background)
                   (set-special 232 background)
                   (set-special 256 text)
                   (set-special 257 background)))
     "")))

(define* (reload-terminal-colors palette #:optional (overrides '()))
  "Updates the colors of all open terminals using the colors in PALETTE.
By default, the colors 0-15 (directly from pywal) will be used, including
the special @code{'background} and @code{'text} colors. Each color that
is used can be overridden by specifying an additional OVERRIDES alist.

@example
;; Use default colors from palette
(reload-terminal-colors palette)

;; Override color index 0 and the named background color.
(reload-terminal-colors palette
                        `((0 . \"#FFFFFF\")
                          ('background . \"#000000\")))
@end example
"
  (let ((path "/dev/pts")
        (sequences (create-sequences palette overrides)))
    #~(begin
        (use-modules (ice-9 rdelim))
        (for-each
         (lambda (file)
           (call-with-output-file (string-append #$path "/" file)
             (lambda (port) (display #$sequences port))))
         (scandir #$path (lambda (f) (string->number f)))))))
