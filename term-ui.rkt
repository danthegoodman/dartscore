#lang racket
(require charterm)

(provide render
         renderln
         screen-cols
         screen-rows
         calculate-screen-size
         )

(define cols 80)
(define rows 24)

(define (screen-cols) cols)
(define (screen-rows) rows)

(define (calculate-screen-size)
  (set!-values (cols rows) (charterm-screen-size)))

(define (render . strs)
  (apply charterm-display (map string->bytes/utf-8 strs))
  )

(define (renderln . strs)
  (apply render strs)
  (charterm-newline)
  )