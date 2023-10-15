#lang racket

(define (sum-interval a b)
  (define (iter current sum)
    (if (> current b)
        sum
        (iter (+ current 1) (+ sum current))))

  (if (> a b)
      0
      (iter a 0)))
