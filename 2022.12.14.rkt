#lang racket

;; Funktion hello, die "Hello World" ausgibt
(define (print input)
  (display input)
  (newline)
  )

(print "Hello_World")


;; Funktion fak, die Fakultät berechen
(define (fak x)
  (if (= x 0)
      1
      (* x (fak (- x 1)))
      )
  )

(fak 10)


; fibonacci
(define (fib x)
  (if (= x 0)
      0
      (if (= x 1)
          1
          (+ (fib (- x 1)) (fib (- x 2)))
          )
      )
  )

(fib 0)
(fib 1)
(fib 5)
(fib 10)
(fib 20)
(fib 30)