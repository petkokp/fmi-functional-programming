#lang racket

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (!! n)
  (define (double-factorial-accumulator acc i)
    (if (<= i n)
        (double-factorial-accumulator (* acc i) (+ i 2))
        acc))
  
  (double-factorial-accumulator 1 (if (even? n) 2 1)))