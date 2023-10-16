#lang racket

(define (prime? n)
  (cond
    ((<= n 1) #f) ; 0 и 1 не се считат за прости числа.
    ((= n 2) #t)  ; 2 е просто число.
    ((= 0 (remainder n 2)) #f) ; Всички четни числа (освен 2) не са прости.
    (else (is-prime n 3))))

(define (is-prime n divisor)
  (cond
    ((> (* divisor divisor) n) #t) ; Достигнали сме до квадрата на n, числото е просто.
    ((= 0 (remainder n divisor)) #f) ; Числото се дели на текущия делител, следователно не е просто.
    (else (is-prime n (+ divisor 2))))) ; Проверяваме само нечетни делители, за да оптимизираме.
