#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (2^ n)
  (accumulate
   *
   1
   1
   n
   (lambda (i) 2)
   (lambda (i) (+ i 1))
   )
  )