#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (divisors-sum n)
  (accumulate
   +
   0
   1
   n
   (lambda (i) (if (= (remainder n i) 0) i 0))
   (lambda (i) (+ i 1))
   )
  )