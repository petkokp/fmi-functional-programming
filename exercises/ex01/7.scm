#lang racket

(define (increasing? n)
  (define (increasing-str? str)
    (if (or (string=? str "")
            (string<? str (substring str 1)))
        #t
        #f))

  (increasing-str? (number->string n)))
