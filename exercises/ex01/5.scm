#lang racket

(define (perfect? n)
  (define (sum-of-divisors k)
    (define (iter divisor total)
      (if (= divisor 0)
          total
          (if (= (remainder k divisor) 0)
              (iter (- divisor 1) (+ total divisor))
              (iter (- divisor 1) total))))
    (iter (- k 1) 0))

  (= (sum-of-divisors n) n))