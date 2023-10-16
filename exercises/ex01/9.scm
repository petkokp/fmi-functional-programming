#lang racket

(define (toDecimal n)
  (if (= n 0)
      0
      (+ (remainder n 10)
         (* 2 (toDecimal (quotient n 10))))))
