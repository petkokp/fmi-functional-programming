#lang racket

(define (const c)
  (lambda (x) c))

(define (flip f)
  (lambda (x y) (f y x)))

(define (complement p)
  (lambda (x) (not (p x))))

(define (compose f g)
  (lambda (x) (f (g x))))