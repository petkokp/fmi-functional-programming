#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (id x) x)

(define (repeat n f)
  (if (= n 0) id
      (compose f (repeat (- n 1) f))))
      ;(lambda (x) (f ((repeat (- n 1) f) x)))))

(define dx 0.000001)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (derive-n n f)
  ((repeat n derive) f))