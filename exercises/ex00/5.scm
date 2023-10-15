#lang racket

(define (count-digits n)
  (string-length (number->string n)))
