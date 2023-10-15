#lang racket

(define (count-digit d n)
  (define (count-helper d n count)
    (if (= n 0)
        count
        (if (= (remainder n 10) d)
            (count-helper d (quotient n 10) (+ count 1))
            (count-helper d (quotient n 10) count))))

  (count-helper d n 0))
