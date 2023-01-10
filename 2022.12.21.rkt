#lang racket

; insert
(define (insert n lst)
  (cond ((null? lst) (list n))
        ((< n (car lst)) (cons n lst))
        (else (cons (car lst)
               (insert n (cdr lst))
               )
        )
  )
)

(insert 3 '(1 2 3 4))

(define (isort lst)
  (if (null? lst)
      lst
      (insert (car lst) (isort (cdr lst)))
  )
)

(isort '(5 6 10 3 4))

(define (isort2 lst o)
  (if (null? lst)
      lst
      (cond (= o >) (insert (car lst) (isort2 (cdr lst)))
            (= o <) (insert (isort2 (cdr lst)) (car lst))
      )
  )
)


(isort2 '(2 5 3 1) <)
(isort2 '(2 5 3 1) >)
;(isort2 '("Hallo" "Tschuess" "Ende") string >?)