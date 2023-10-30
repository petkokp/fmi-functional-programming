#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))
      ;(lambda (x) (f ((repeat (- n 1) f) x)))))

(define (twist k f g)
  (repeat (/ k 2) (compose f g)))