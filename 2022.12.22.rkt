#lang racket

; left: Return the even-numbered elements of lst (starting with 0)
(define (split1 lst)
  (if (null? lst)
      lst
      (if (null? (cdr lst))
          lst
          (cons (car lst) (split1 (cdr (cdr lst)))))))

;;; right: Return the odd-numbered elements of lst (starting with 1)
(define (split2 lst)
  (if (null? lst)
      lst
      (if (null? (cdr lst))
          '()
          (cons (car (cdr lst)) (split2 (cdr (cdr lst)))))))


(define (split lst)
  (list (split1 lst) (split2 lst)))

(split1 '(1 2 3 4 5 6 7 8))
(split1 '(1 2 3 4 5 6 7))
(split1 '(1))

(split2 '(1 2 3 4 5 6 7 8))
(split2 '(1 2 3 4 5 6 7))
(split2 '(1))

(split '(1 2 3 4 5 6 7 8))
(split '(1 2 3 4 5 6 7))
(split '(1))
(split '())



(cons 1 2)
(cons '() '())
(cons 1 '())
(cons 1 (cons 2 (cons 3 '())))
'(1 . 2)
'(1 . (2 . (3 . ())))

; unechte liste
'(1 . (2 . (3 . 4)))