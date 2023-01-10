#lang racket

; element anfagns hinzufügen
(cons 1 '(2 3 4))

; zwei listen kombinieren
(append '(1 2 3) '(4 5) )

; erste element rückgabe
(car '(1 2 3))

; erstes element wird gelöscht
(cdr '(1 2 3))

; checkt ob liste leer ist
(null? '())

; list umdrehen
(define (revert l)
  (if (null? l)
      #t
      (append (revert (cdr l)) (list (car l)))
      )
  )


;(revert '(1 2 3))


; scalar multiplikation
(define (scalar-mult l faktor)
   (if (null? l)
      #t
      (const (* (car l) faktor) (scalar-mult (cdr l) faktor))
      ) 
  )

;(scalar-mult '(1 2 3) 5)
;(scalar-mult '(7 11 13) 0.5)
;(scalar-mult '() 42)


(define (is-element? element lst)
  (if (null? lst)
      #f
      (if (equal? element (car lst))
          #t
          (is-element? element (cdr lst))
      )
  )
)

; insert
(define (insert n lst)
  (if(is-element? n lst)
     lst
     (append lst (list n))
  )
)

;(insert 3 '(1 2 3 4))
;(insert 5 '(1 2 3 4))




