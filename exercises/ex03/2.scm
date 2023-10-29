#lang racket

(define dx 0.000001)

(define (derive f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))