#lang racket

; skalarprodukt
(define (skalarprodukt l1 l2)
    (apply + (map * l1 l2))
)

(skalarprodukt '(1 2 3) '(2 4 6))


; flattern
(define (flattern l)
    (if (pair? l) ; ist l ein cons paar?
        (apply append (map flattern l)) ; wenn cons paar: elemente werden rekuursiv aneinander gefügt
        (list l)) ; wenn kein cons paar: element wird in liste geschrieben
)

(flattern '(1 2 (3 4 (5 6) 7 8) 9))