#lang racket

(define (reverse-digits n)
  (define (reverse-helper n result)
    (if (zero? n)
        result
        (reverse-helper (quotient n 10) (+ (* result 10) (remainder n 10)))))

  (reverse-helper n 0))

(reverse-digits 321)