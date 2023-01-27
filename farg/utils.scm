(define-module (farg utils)
  #:use-module (srfi srfi-1)
  #:use-module (guix gexp)
  #:use-module (gnu services))

(define-public (list-of-services? lst)
  (every service? lst))

(define-public (maybe-string? str)
  (or (string? str) (eq? str #f)))

(define-public (serialize-string str)
  (if str str ""))

(define-public (serialize-boolean bool)
  (number->string
   (if bool 1 0)))

;; Field type for allowing either a number or a procedure
;; returning a number based on same argument. This can be used for e.g.
;; setting saturation or alpha dynamically based on light/dark themes.
(define-public (number-or-getter? x)
  (or (number? x)
      (and (procedure? x)
           (>= (car (procedure-minimum-arity x)) 1))))

(define-public (wallpaper-or-getter? x)
  (or (or (string? x) (eq? x #f))
      (and (procedure? x)
           (>= (car (procedure-minimum-arity x)) 1))))

(define-public (maybe-palette-getter? proc)
  (or (eq? proc #f)
      (and (procedure? proc)
           (eq? (car (procedure-minimum-arity proc)) 1))))
