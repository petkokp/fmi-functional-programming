#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (all? p? a b)
  (accumulate
   (lambda (x y) (and x y))
   #t
   a
   b
   (lambda (x) (p? x))
   (lambda (i) (+ i 1))
   )
  )

(define (any? p? a b)
  (accumulate
   (lambda (x y) (or x y))
   #t
   a
   b
   (lambda (x) (p? x))
   (lambda (i) (+ i 1))
   )
  )