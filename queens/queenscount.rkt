#lang racket
;;; n Queens problem
;;; config is a list of placement, with the kth queen at 
;;; the specified position  of the kth-last element. 
;;; The empty board  corresponds to the empty list

(define (diag-check? config pos distance)
  (cond ((null? config)
         #t)
        ((= (abs (- pos (car config))) distance)
         #f)
        (else
         (diag-check? (cdr config) pos (+ distance 1)))))
         
(define (queen-possible? config pos)
  (cond ((member pos config) 
         #f)
        (else 
         (diag-check? config pos 1))))
  
;;; n: total number of queens. pos: Current queen to be tried

(define (try-queens config n pos)1
  ;(display "Trying:")(newline)(print-board config n)(newline)
  (cond ((> pos n)
         '())
        (#t
         (append (if (queen-possible? config pos) 
                     (solve-n-queens-rek (cons pos config) n)
                     '())
                 (try-queens config n (+ pos 1))))))

(define (solve-n-queens-rek config n)
  (cond ((= (length config) n)
         (list config))
        (else
         (try-queens config n 1))))

(define (print-row n pos)
  (if (= pos 1)
      (display "Q")
      (display "#"))
  (if (= n 1)
      (newline)
      (print-row (- n 1) (- pos 1))))

(define (print-board conf n)
  (cond ((null? conf)
         )
        (else
         (print-row n (car conf))
         (print-board (cdr conf) n))))        
         
(define (solve-n-queens n)
  (let ((solutions (solve-n-queens-rek '() n)))
    (display "Number of solutions: ")
    (display (length solutions))
    (newline)))
           
(solve-n-queens 13)