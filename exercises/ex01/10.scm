#lang racket

(define (fastExponentiation x n)
  (define (sq y)
    (* y y))
  (if (= n 0)
      1
      (if (even? n)
          (sq (fastExponentiation x (quotient n 2)))
          (* x (sq (fastExponentiation x (quotient n 2)))))))
