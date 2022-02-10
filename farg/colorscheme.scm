(define-module (farg colorscheme)
  #:use-module (farg utils)
  #:use-module (farg config)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (gnu services configuration)
  #:export (
            colorscheme
            <colorscheme>
            colorscheme?
            colorscheme-light?
            colorscheme-wallpaper
            colorscheme-primary
            colorscheme-primary-text
            colorscheme-secondary
            colorscheme-secondary-text
            colorscheme-background

            hex->hsl
            hex->rgba
            hex->luminance
            contrast

            colors->colorscheme
            generate-colorscheme
            read-generated-colorscheme))

(define-configuration
  colorscheme
  (light?
   (boolean #f)
   "If the colorscheme is a light theme")
  (wallpaper
   (maybe-string #f)
   "Colorscheme wallpaper")
  (primary
   (string "")
   "Primary accent color")
  (secondary
   (string "")
   "Secondary accent color")
  (text
   (string "")
   "Main text color")
  (primary-text
   (string "")
   "Primary accent complementary text color")
  (secondary-text
   (string "")
   "Secondary accent complementary text color")
  (background
   (string "")
   "Main background color")
  (no-serialization))

;; TODO: Add color conversion helpers, e.g. hex->rgb.
;; TODO: Add color manipulation helpers, e.g. darken, lighten, saturate, adjust-contrast, etc.

(define (generate-colorscheme config)
  (system (string-join
           (list "$(guix build python-pywal)/bin/wal"
                 "-i" (farg-config-wallpaper config)
                 "--backend" (farg-config-backend config)
                 "--saturate" (number->string (farg-config-saturation config))
                 (if (farg-config-light? config) "-l" "")
                 ;; Skip reloading
                 "-e" "-t" "-s" "-n")
           " ")))

(define* (read-generated-colorscheme)
  "Read generated colors from pywal cache."
  (define file
    (string-append
     (or (getenv "XDG_CACHE_HOME")
         (string-append (getenv "HOME")  "/.cache"))
     "/wal/colors"))

  (define (read-colors port acc index)
    (let ((color (read-line port)))
      (if (eof-object? color)
          acc
          (read-colors port
                       (cons `(,index . ,color) acc)
                       (+ index 1)))))

 (read-colors (open-input-file file) '() 0))

(define* (colors->colorscheme colors config)
  "Converts a list of generated colors into a colorscheme record."
  ;; TODO: Correctly set primary and secondary text.
  ;; TODO: Generate extra color for light theme background
  (if (farg-config-light? config)
      ;; Light theme template
      (colorscheme
       (light? (farg-config-light? config))
       (wallpaper (farg-config-wallpaper config))
       (primary (assoc-ref colors 3))
       (secondary (assoc-ref colors 5))
       (text (assoc-ref colors 16))
       (background (assoc-ref colors 7))
       (primary-text (assoc-ref colors 8))
       (secondary-text (assoc-ref colors 9)))

      ;; Dark theme template
      (colorscheme
       (light? (farg-config-light? config))
       (wallpaper (farg-config-wallpaper config))
       (primary (assoc-ref colors 10))
       (secondary (assoc-ref colors 13))
       (text (assoc-ref colors 15))
       (background (assoc-ref colors 0))
       (primary-text (assoc-ref colors 8))
       (secondary-text (assoc-ref colors 9)))))

(define* (hex->rgba str #:key (alpha? #t))
  "Converts a hex color STR into its RGBA color representation.
If the hex color does not specify the alpha, it will default to 100%."
  (define (split-rgb acc hex)
    (if (eq? (string-length hex) 0)
        acc
        (split-rgb
         (cons (exact->inexact (/ (string->number (string-take hex 2) 16) 255)) acc)
         (string-drop hex 2))))

  (let* ((hex (substring str 1))
         (rgb (split-rgb '() hex))
         (has-alpha? (eq? (length rgb) 4)))
    (reverse
     (if alpha?
         (if has-alpha? (cons 1.0 rgb) rgb)
         (if has-alpha? (list-tail rgb 1) rgb)))))

(define* (rgb->hsl rgb)
  "Converts RGB into its HSL color representation."
  (define (safe-division x1 x2 denom)
    (if (= denom 0.0)
        0.0
        (/ (- x1 x2) denom)))

  (let* ((c-min (apply min rgb))
         (c-max (apply max rgb))
         (lum (/ (+ c-min c-max) 2))
         (sat (if (<= lum 0.5)
                  (safe-division c-max c-min (+ c-max c-min))
                  (safe-division c-max c-min (- 2.0 c-max c-min))))
         (hue-denom (- c-max c-min))
         (hue (if (= hue-denom 0.0)
                  ;; Hue is undefined in cases where the denominator is 0.
                  0.0
                  (* 60.0
                     (match (list-index (lambda (v) (eq? v c-max)) rgb)
                       ;; Red
                       (0 (safe-division (list-ref rgb 1)
                                         (list-ref rgb 2)
                                         hue-denom))
                       ;; Green
                       (1 (+ (safe-division (list-ref rgb 2)
                                            (list-ref rgb 0)
                                            hue-denom)
                             2.0))
                       ;; Blue
                       (2 (+ (safe-division (list-ref rgb 0)
                                            (list-ref rgb 1)
                                            hue-denom)
                             4.0)))))))
    `(,(if (negative? hue) (+ hue 360) hue) ,sat ,lum)))

(define* (hex->hsl hex)
  "Converts a hex color HEX into its HSL color representation.
Conversion of black and white will result in a hue of 0% (undefined)."
  (rgb->hsl (hex->rgba hex #:alpha? #f)))

(define* (hsl->hex hsl)
  "Converts HSL into its hex color representation."
  hsl)

;; Based on formula at https://www.myndex.com/WEB/LuminanceContrast.
(define* (hex->luminance hex)
  "Calculates the luminance of hex color HEX."
  (apply + (map (lambda (pair) (* (car pair) (expt (cadr pair) 2.2)))
                (zip '(0.2126 0.7152 0.0722)
                     (hex->rgba hex #:alpha? #f)))))

;; Based on https://github.com/protesilaos/modus-themes/blob/main/modus-themes.el.
(define* (contrast c1 c2)
  "Calculates the WCAG contrast ratio between the hex colors C1 and C2."
  (let ((ct (/ (+ (hex->luminance c1) 0.05)
               (+ (hex->luminance c2) 0.05))))
    (max ct (/ ct))))

(define* (bounded lower upper value)
  "Bounds VALUE between LOWER and UPPER."
  (min upper (max lower value)))

(define* (adjust-luminance hex percentage proc)
  "Adjusts the luminance of HEX by applying PERCENTAGE
and current luminance to PROC."
  (let ((hsl (hex->hsl hex)))
    `(,(car hsl)
      ,(cadr hsl)
      ,(bounded 0.0 1.0 (proc (caddr hsl)
                              (/ percentage 100))))))

(define* (lighten hex percentage)
  "Increases the luminance of hex color HEX by PERCENTAGE."
  (hsl->hex (adjust-luminance hex percentage +)))

(define* (darken hex percentage)
  "Decreases the luminance of hex color HEX by PERCENTAGE."
  (hsl->hex (adjust-luminance hex percentage -)))
