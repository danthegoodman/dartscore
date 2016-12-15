#lang racket
(module+ test (require rackunit))
(provide color
         COLOR_LEN)

(define (color clrs str)
  (string-append (colors-start 39 49 clrs) str "\u1b[m"))

(define (colors-start fg bg clrs)
  (if (empty? clrs) (~a "\u1b[" fg ";" bg "m")
      (match (first clrs)
        ['fg-red         (colors-start 31 bg (rest clrs))]
        ['fg-green       (colors-start 32 bg (rest clrs))]
        ['fg-yellow      (colors-start 33 bg (rest clrs))]
        ['fg-cyan        (colors-start 36 bg (rest clrs))]
        ['fg-dark-gray   (colors-start 90 bg (rest clrs))]
        ['bg-green       (colors-start fg 42 (rest clrs))]
        )))

(define COLOR_LEN (string-length (color '() "")))

(module+ test
  (check-equal? "\u1b[32;49mbar\u1b[m" (color '(fg-green) "bar"))
  (check-equal? "\u1b[32;42mbar\u1b[m" (color '(fg-green bg-green) "bar"))  
  (check-equal? "\u1b[39;42mbar\u1b[m" (color '(bg-green) "bar"))  
  (check-equal? "\u1b[39;49mbar\u1b[m" (color '() "bar"))  

  )