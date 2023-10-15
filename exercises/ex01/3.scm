#lang racket

(define (reverse-digits n)
  (define (reverse-helper n reversed)
    (if (= n 0)
        reversed
        (reverse-helper (quotient n 10) (+ (* reversed 10) (remainder n 10)))))

  (reverse-helper n 0))
