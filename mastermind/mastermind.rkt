#lang racket

;;; Define the game

(define no-of-pins    4)
(define no-of-colours 6)

;;; Helper functions

(define (uniq lst)
  (if (null? lst)
      '()
      (let ((element (car lst))
            (cdr-uniq (uniq (cdr lst))))
        (if (member element cdr-uniq)
            cdr-uniq
            (cons element cdr-uniq)))))

;;; Compare two (implicitly equal length) lists, 
;;; return the number of positions at which both
;;; list have the same element

(define (exact-matches l1 l2)
  (if (null? l1)
      0
      (+
       (if (equal? (car l1) (car l2))
           1
           0)
       (exact-matches (cdr l1) (cdr l2)))))

;;; Remove element from list (if it occurs)

(define (delete-element e lst)
  (cond ((null? lst)
         '())
        ((equal? e (car lst))
         (cdr lst))
        (else
         (cons (car lst)
               (delete-element e (cdr lst))))))

;;; Count occurrances of same elements of l1 in l2

(define (count-occ l1 l2)
  (if (null? l1)
      0
      (if (member (car l1) l2)
          (+
           1
           (count-occ (cdr l1) (delete-element (car l1) l2)))
          (count-occ (cdr l1) l2))))

;;; Evaluate a MasterMind guess vs the solution. Return value is a list with
;;; number of exact matches, number of correct colours

(define (mm-eval guess solution)
  (let ((exact (exact-matches guess solution)))
    (list
     exact
     (- (count-occ guess solution) exact))))

;;; For convenience:

(define (winning-eval? ev)
  (= (car ev) no-of-pins))

;;; Given a tuple and an integer "colours", generate a list of tuples by
;;; prepending the tuple with each integer value from 1 to "colours"

(define (add-to-tuple tuple colours)
  (if (= colours 0)
      '()
      (cons (cons colours tuple)
            (add-to-tuple tuple (- colours 1)))))

;;; Given a set of tuples, and a set of coulours (represented as the number
;;; of the biggest colour), prepend each tuple with each colour and return
;;; the resulting list of tuples.

(define (add-to-tuples tuples colours)
  (if (null? tuples)
      '()
      (append (add-to-tuple (car tuples) colours)
              (add-to-tuples (cdr tuples) colours))))

;;; Given a number of pins and a number of colours, generate all compatible
;;; tuples

(define (make-guesses pins colours)
  (if (= pins 0)
      (list '())
      (let ((short-tuples (make-guesses (- pins 1) colours)))
        (add-to-tuples short-tuples colours))))

;;; Make all tuples for the game as configured

(define (make-all-guesses)
  (sort
   (make-guesses no-of-pins no-of-colours)
   (lambda (x y) (> (length (uniq x)) (length (uniq y))))
   ))


;;; Generic filter

(define (filter lst filter-fun)
  (if (null? lst)
      '()
      (if (filter-fun (car lst))
          (cons (car lst) (filter (cdr lst) filter-fun))
          (filter (cdr lst) filter-fun))))

;;; Return true iff the solution candidate results in evaluation ev on guess.

(define (is-compatible candidate guess ev)
  (equal? (mm-eval guess candidate) ev))

;;;

(define (filter-compatible candidates guess ev)
  (filter candidates (lambda (x) (is-compatible x guess ev))))
     
;;;

(define (solve-mm solution candidates)
  (cond ((null? candidates)
         (display "I'm giving up")
         (newline)
         #f)
        (else
         (let* ((guess (car candidates))
                (ev    (mm-eval guess solution)))
           (display "Guess: ")
           (display guess)
           (display " -> ")
           (display ev)
           (newline)
           (if (winning-eval? ev)
               guess
               (solve-mm solution (filter-compatible candidates guess ev)))))))
      
;;;

(define (master-mind solution)
  (display "Game initialized. Secret code: ")
  (display solution)
  (newline)
  (solve-mm solution (make-all-guesses)))


(define (master-mind-interactive)
  (master-mind (read))
  (master-mind-interactive))


(exact-matches '() '())
(exact-matches '(1 2 3) '(1 2 3))
(exact-matches '(2 1 3) '(1 2 3))

(delete-element 1 '())
(delete-element 1 '(1))
(delete-element 1 '(2))
(delete-element 1 '(2 1 3))

(count-occ '() '())
(count-occ '(1 2 3) '(3 2 1))
(count-occ '(1 1 1) '(1 2 3))

(mm-eval '(1 2 3 4) '(1 2 3 4))
(mm-eval '(1 2 3 4) '(4 3 2 1))
(mm-eval '(1 2 3 4) '(1 1 1 1))

(add-to-tuple '() 6)
(add-to-tuple '(1 2 3) 6)

(length (make-all-guesses))

(filter-compatible (make-all-guesses) '(1 2 3 4) '(2 2))

(uniq '(1 2 3 1 1 4))


(master-mind '(1 2 3 4))
(master-mind '(4 2 2 1))
(master-mind '(3 3 3 3))

(master-mind-interactive)