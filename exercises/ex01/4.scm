#lang racket

(define (divisors-sum n)
  (define (divisor-sum-helper current sum)
    (if (= current 0)
        sum
        (if (= (remainder n current) 0)
            (divisor-sum-helper (- current 1) (+ sum current))
            (divisor-sum-helper (- current 1) sum))))

  (divisor-sum-helper n 0))
