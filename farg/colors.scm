(define-module (farg colors)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (farg utils)
  #:export (
            farg:hex->hsl
            farg:hex->rgba
            farg:hex->luminance
            farg:rgba->luminance
            farg:rgba->hex
            farg:rgba->hsl
            farg:hsl->hex
            farg:hsl->rgba

            farg:with-alpha
            farg:apply-filters

            farg:lighten
            farg:darken
            farg:brighten
            farg:saturate
            farg:desaturate
            farg:offset
            farg:blend
            farg:make-readable
            farg:get-contrast
            farg:get-contrast/rgba
            farg:get-contrast/hsl
            farg:lighten/hsl
            farg:darken/hsl
            farg:brighten/hsl
            farg:saturate/hsl
            farg:desaturate/hsl))

(define* (bounded lower upper value)
  "Bounds VALUE between LOWER and UPPER."
  (min upper (max lower value)))

(define* (set-alpha prev new)
  "Mirrors the alpha channel of PREV to NEW. If NEW has an alpha
channel, but PREV does not, it will be removed."
  (let ((prev-alpha? (eq? (string-length prev) 9))
        (new-alpha? (eq? (string-length new) 9)))
    (cond
     ((and (not prev-alpha?) new-alpha?) (string-drop-right new 2))
     ((and prev-alpha? (not new-alpha?)) (string-append new (string-take-right prev 2)))
     (else new))))

(define* (adjust-lightness hsl amount proc)
  "Adjusts the lightness of HSL by applying AMOUNT
and current lightness to PROC."
  `(,(car hsl)
    ,(cadr hsl)
    ,(bounded 0.0 1.0 (proc (caddr hsl)
                            (/ amount 100)))))

(define* (adjust-saturation hsl amount proc)
  "Adjusts the saturation of HSL by applying AMOUNT
and current saturation to PROC."
  `(,(car hsl)
    ,(bounded 0.0 1.0 (proc (cadr hsl)
                            (/ amount 100)))
    ,(caddr hsl)))

(define* (djust-hue hsl amount proc)
  "Adjusts the hue of HSL by applying AMOUNT and current hue to PROC."
  `(,(bounded 0.0 1.0 (proc (car hsl)
                            (/ amount 100)))
    ,(cadr hsl)
    ,(caddr hsl)))

(define* (farg:with-alpha hex alpha)
  "Sets the alpha channel of HEX based on the percentage ALPHA.
If HEX has an alpha set, it will be replaced."
  (let ((new-alpha (format #f "~2,'0x" (inexact->exact (round (* 255.0 (/ alpha 100)))))))
    (if (eq? (string-length hex) 9)
        (string-append (string-take hex 7) new-alpha)
        (string-append hex new-alpha))))

(define* (farg:hex->rgba str #:key (alpha? #f))
  "Converts a hex color STR into its RGBA color representation.
If the hex color does not specify the alpha, it will default to 100%."
  (define (split-rgb acc hex)
    (if (eq? (string-length hex) 0)
        acc
        (split-rgb
         (cons (exact->inexact (/ (string->number (string-take hex 2) 16) 255)) acc)
         (string-drop hex 2))))

  (if (or (not (string? str))
          (eq? (string-length str) 0))
      (throw-error (format #f "'~a' is not a valid hex color" str))
      (let* ((hex (if (equal? (string-take str 1) "#")
                      (substring str 1)
                      str))
             (rgb (split-rgb '() hex))
             (has-alpha? (eq? (length rgb) 4)))
        (reverse
         (if alpha?
             (if has-alpha? (cons 1.0 rgb) rgb)
             (if has-alpha? (list-tail rgb 1) rgb))))))

(define* (farg:hex->hsl hex)
  "Converts a hex color HEX into its HSL color representation.
Conversion of black and white will result in a hue of 0% (undefined)."
  (farg:rgba->hsl (farg:hex->rgba hex)))

;; Based on formula at https://www.myndex.com/WEB/LuminanceContrast.
(define* (farg:hex->luminance hex)
  "Calculates the luminance of hex color HEX."
  (farg:rgba->luminance (farg:hex->rgba hex)))

(define* (farg:rgba->luminance color)
  "Calculates the luminance of COLOR in rgba format."
  (define (calc pair)
    (let ((value (cadr pair)))
      (* (car pair)
         ;; FIXME: Some colors, e.g. #00FF00 or #0000FF will yield complex numbers.
         ;; Not sure how to fix, so just replace it with 0 and be done with it.
         (if (> (imag-part value) 0)
             0
             (expt value 2.2)))))

  (apply + (map calc (zip '(0.2126 0.7152 0.0722) color))))

(define* (farg:rgba->hex rgba #:key (alpha? #f))
  "Converts RGBA into its hex color representation."
  (fold
   (lambda (v acc)
     (string-append
      acc
      (format #f "~2,'0x" (bounded 0 255 (inexact->exact (round (* v 255)))))))
   "#"
   (if (or alpha? (= (length rgba) 3))
       (append rgba '(1.0))
       rgba)))

(define* (farg:rgba->hsl rgba)
  "Converts RGBA into its HSL color representation."
  (define (safe-division x1 x2 denom)
    (if (= denom 0.0)
        0.0
        (/ (- x1 x2) denom)))

  (let* ((c-min (apply min rgba))
         (c-max (apply max rgba))
         (lum (/ (+ c-min c-max) 2))
         (sat (if (<= lum 0.5)
                  (safe-division c-max c-min (+ c-max c-min))
                  (safe-division c-max c-min (- 2.0 c-max c-min))))
         (hue-denom (- c-max c-min))
         (hue (if (= hue-denom 0.0)
                  ;; Hue is undefined in cases where the denominator is 0.
                  0.0
                  (* 60.0
                     (match (list-index (lambda (v) (eq? v c-max)) rgba)
                       ;; Red
                       (0 (safe-division (list-ref rgba 1)
                                         (list-ref rgba 2)
                                         hue-denom))
                       ;; Green
                       (1 (+ (safe-division (list-ref rgba 2)
                                            (list-ref rgba 0)
                                            hue-denom)
                             2.0))
                       ;; Blue
                       (2 (+ (safe-division (list-ref rgba 0)
                                            (list-ref rgba 1)
                                            hue-denom)
                             4.0)))))))
    `(,(if (negative? hue) (+ hue 360) hue) ,sat ,lum)))

(define* (farg:hsl->rgba hsl)
  "Convert HSL into its RGBA color representation."
  (define hue (list-ref hsl 0))
  (define sat (list-ref hsl 1))
  (define lum (list-ref hsl 2))

  (define (normalize-rgb-value v)
    (if (negative? v)
        (+ v 1)
        (if (> v 1) (- v 1) v)))

  (if (= sat 0.0)
      ;; Shade of grey, convert to RGB directly.
      (map (lambda (v) (* v lum)) `(1.0 1.0 1.0))
      (let* ((magic1 (if (< lum 0.5)
                         (* lum (+ 1.0 sat))
                         (- (+ lum sat) (* lum sat))))
             (magic2 (- (* 2.0 lum) magic1))
             (hue-norm (/ hue 360))
             (tmp-r (normalize-rgb-value (+ hue-norm 0.3333)))
             (tmp-g (normalize-rgb-value hue-norm))
             (tmp-b (normalize-rgb-value (- hue-norm 0.3333))))
        (map (lambda (v)
               (cond
                ((< (* 6 v) 1.0) (+ magic2 (* 6.0 v (- magic1 magic2))))
                ((< (* 2 v) 1.0) magic1)
                ((< (* 3 v) 2.0) (+ magic2 (* 6.0 (- 0.6666 v) (- magic1 magic2))))
                (else magic2)))
         (list tmp-r tmp-g tmp-b)))))

(define* (farg:hsl->hex hsl)
  "Converts HSL into its hex color representation."
  (farg:rgba->hex (farg:hsl->rgba hsl)))

(define (farg:get-contrast/rgba c1 c2)
  "Calculates the WCAG contrast ratio between the RGBA colors C1 and C2."
  (let ((ct (/ (+ (farg:rgba->luminance c1) 0.05)
               (+ (farg:rgba->luminance c2) 0.05))))
    (if (> (imag-part ct) 0)
        0
        (max ct (/ ct)))))

(define (farg:get-contrast/hsl c1 c2)
  "Calculates the WCAG contrast ratio between the HSL colors C1 and C2."
  (farg:get-contrast/rgba (farg:hsl->rgba c1)
                          (farg:hsl->rgba c2)))

;; Based on https://github.com/protesilaos/modus-themes/blob/main/modus-themes.el.
(define* (farg:get-contrast c1 c2)
  "Calculates the WCAG contrast ratio between the hex colors C1 and C2."
  (farg:get-contrast/rgba (farg:hex->rgba c1)
                      (farg:hex->rgba c2)))

(define* (farg:brighten/hsl hsl amount)
  (farg:rgba->hsl (map (lambda (v) (bounded 0 255 (+ v (/ amount 100))))
                       (farg:hsl->rgba hsl))))

(define* (farg:lighten/hsl hsl amount)
  (adjust-lightness hsl amount +))

(define* (farg:darken/hsl hsl amount)
  (adjust-lightness hsl amount -))

(define* (farg:saturate/hsl hsl amount)
  (adjust-saturation hsl amount +))

(define* (farg:desaturate/hsl hsl amount)
  (adjust-saturation hsl amount -))

(define* (farg:brighten hex #:optional (amount 10))
  "Decreases the brightness of hex color HEX by AMOUNT."
  (set-alpha hex
             (farg:rgba->hex
              (map (lambda (v) (bounded 0 255 (+ v (/ amount 100))))
                   (farg:hex->rgba hex)))))

(define* (farg:lighten hex #:optional (amount 10))
  "Increases the lightness of hex color HEX by AMOUNT."
  (set-alpha hex (farg:hsl->hex (farg:lighten/hsl (farg:hex->hsl hex) amount))))

(define* (farg:darken hex #:optional (amount 10))
  "Decreases the lightness of hex color HEX by AMOUNT."
  (set-alpha hex (farg:hsl->hex (farg:darken/hsl (farg:hex->hsl hex) amount))))

(define* (farg:saturate hex #:optional (amount 10))
  "Increases the saturation of hex color HEX by AMOUNT."
  (set-alpha hex (farg:hsl->hex (farg:saturate/hsl (farg:hex->hsl hex) amount))))

(define* (farg:desaturate hex #:optional (amount 10))
  "Decreases the saturation of hex color HEX by AMOUNT."
  (set-alpha hex (farg:hsl->hex (farg:desaturate/hsl (farg:hex->hsl hex) amount))))

(define* (farg:offset hex #:optional (amount 10))
  "Adjust the brightness of hex color HEX by AMOUNT, based on whether
or not HEX is a dark or light color. For light colors, a darker color
will be generated, and for dark colors, a lighter color will be generated."
  (let* ((hsl (farg:hex->hsl hex))
         (lum (caddr hsl)))
    (set-alpha hex
               (farg:hsl->hex
                ;; Check if color is bright or dark
                (if (> lum 0.5)
                    (farg:darken/hsl hsl amount)
                    (farg:brighten/hsl hsl amount))))))

(define* (farg:blend source backdrop #:optional (percentage 0.9))
  "Blends SOURCE with percentage PERCENTAGE with BACKDROP.
Setting PERCENTAGE >= 1.0 will return SOURCE, and PERCENTAGE = 0 will return BACKDROP."
  (let* ((source-rgb (farg:hex->rgba source))
         (backdrop-rgb (farg:hex->rgba backdrop))
         (mR (- (car backdrop-rgb) (car source-rgb)))
         (mG (- (cadr backdrop-rgb) (cadr source-rgb)))
         (mB (- (caddr backdrop-rgb) (caddr source-rgb))))
    (set-alpha source
               (farg:rgba->hex
                `(,(+ (* mR (- 1 percentage)) (car source-rgb))
                  ,(+ (* mG (- 1 percentage)) (cadr source-rgb))
                  ,(+ (* mB (- 1 percentage)) (caddr source-rgb)))))))

(define* (farg:make-readable fg bg #:optional (ratio 7))
  "Calculates a new color based on hex color FG that has a contrast ratio
of RATIO to the hex color BG."
  (define step 5) ;; 5% lightness change per iteration
  (define bg-hsl (farg:hex->hsl bg))
  (define bg-light? (> (caddr bg-hsl) 0.5))

  (define (find-readable-color hsl)
    (let* ((fg-lightness (caddr hsl)))
      (if (or (>= fg-lightness 0.95)
              (<= fg-lightness 0.05)
              (>= (farg:get-contrast/hsl hsl bg-hsl) ratio))
          hsl
          (find-readable-color (if bg-light?
                                   (farg:darken/hsl hsl step)
                                   (farg:lighten/hsl hsl step))))))

  (set-alpha fg
             (farg:hsl->hex (find-readable-color (farg:hex->hsl fg)))))

(define* (farg:apply-filters hex filters)
  "Applies the filters in FILTERS to HEX. Only converts between color representations once, thus yielding better performance.
@example
(farg:apply-filters \"#000000\"
                    '((farg:lighten 20)
                      (farg:saturate 20)
                      (farg:brighten 10)))
@end example"
  (set-alpha
   hex
   (farg:hsl->hex
    (fold (lambda (filter acc)
            (apply (primitive-eval (symbol-append 'hsl: (car filter)))
                   (let ((args (list-tail filter 1)))
                     `(,acc ,@(if (null? args) 10 args)))))
            (farg:hex->hsl hex)
            filters))))
