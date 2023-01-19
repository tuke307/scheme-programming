#lang racket

;;; Define the game

(define no-of-pins    4)
(define no-of-colours 6)

;;; Helper functions

;;; Remove duplicates from a sorted list

(define (sorted-uniq lst)
  (cond ((null? lst)
         lst)
        ((null? (cdr lst))
         lst)
        ((equal? (car lst) (cadr lst))
         (sorted-uniq (cdr lst)))
        (#t
         (cons (car lst) (sorted-uniq (cdr lst))))))

;;; Sort and uniquify a list

(define (sort-uniq lst)
  (sorted-uniq (sort lst <)))

;;; Number of different colours

(define (diversity lst)
  (length (sort-uniq lst)))
  
;;; Compare two lists based on the number of unique elements

(define (diversity> l1 l2)
  (> (diversity l1) (diversity l2)))

;;; Generic filter

(define (filter lst filter-fun?)
  (if (null? lst)
      '()
      (if (filter-fun? (car lst))
          (cons (car lst) (filter (cdr lst) filter-fun?))
          (filter (cdr lst) filter-fun?))))


;;; Compare two (implicitly equal length) lists, 
;;; return the number of positions at which both
;;; list have the same element

(define (exact-matches-old l1 l2)
  (if (null? l1)
      0
      (+
       (if (equal? (car l1) (car l2))
           1
           0)
       (exact-matches-old (cdr l1) (cdr l2)))))


(define (exact-matches l1 l2)
  (apply + (map (lambda (x y) (if (= x y) 1 0)) l1 l2)))

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


;;; Given an integer number, create a list of numbers up to it

(define (create-palette no-of-colours)
  (if (= no-of-colours 0)
      '()
      (cons no-of-colours (create-palette (- no-of-colours 1)))))

;;; Create a list of all lists of lenght no-of-pins with elements from palette

(define (create-all-candidates no-of-pins palette)
  (if (= no-of-pins 0)
      '(())
      (let ((subres (create-all-candidates (- no-of-pins 1) palette)))
        (apply append (map (lambda (colour) (map (lambda (x) (cons colour x)) subres)) palette)))))


;;; Create candidates for the defined game

(define (create-candidates)
  (create-all-candidates no-of-pins (create-palette no-of-colours)))

;;; Better version: Go for the more diverse candidates first

(define (create-candidates-opt)
  (sort (create-candidates) diversity>))

;;; Try random order

(define (create-candidates-rnd)
  (shuffle (create-candidates)))

;;; Even better? Randomize within each quality class) Depressingly, this is
;;; worse than opt, which is worse than random!

(define (create-filter x)
  (lambda (cand) (equal? (car cand) x)))

(define (create-candidates-opt2)
  (let* ((cands (create-candidates))
         (diversity-map (map diversity cands))
         (ext-cands (map (lambda (cand div) (cons div cand)) cands diversity-map))
         (div-values (reverse (sort-uniq diversity-map)))
         (filter-preds (map create-filter div-values))
         (cands-by-diversity (map (lambda (pred) (filter ext-cands pred)) filter-preds))
         (shuffled-cands (map shuffle cands-by-diversity ))
         )   
    (map cdr (apply append shuffled-cands))
    )
  )
 

          
;(create-candidates-opt2)

;;; Return true iff the solution candidate results in evaluation ev on guess.

(define (is-compatible? candidate guess ev)
  (equal? (mm-eval guess candidate) ev))

;;; Filter a list of candidate guesses against an evaluation

(define (filter-compatible candidates guess ev)
  (filter candidates (lambda (x) (is-compatible? x guess ev))))
     
;;; Solve master mind. Resturn solution (or #f) and number of guesses

(define (solve-mm solution candidates tries)
  (cond ((null? candidates)
         (display "I'm giving up")
         (newline)
         (list #f tries))
        (else
         (let* ((guess (car candidates))
                (ev    (mm-eval guess solution)))
           (display "Guess: ")
           (display guess)
           (display " -> ")
           (display ev)
           (newline)
           (if (winning-eval? ev)
               (list guess tries)
               (solve-mm solution (filter-compatible candidates guess ev) (+ tries 1)))))))
      
;;; Nice wrapper

(define (master-mind solution create-cand)
  (display "Game initialized. Secret code: ")
  (display solution)
  (newline)
  (solve-mm solution (create-cand) 1))

;;; Play "against" the human (who only provides the code)

(define (master-mind-interactive)
  (let ((code (read)))
    (if (not (eof-object? code))
        (begin 
          (master-mind code create-candidates-opt)
          (master-mind-interactive))
        (display "Game over")
        )
    )
  )


;;; Play a game for all the candidates in candidates,
;;; generate guesses for each with function create-cand, return
;;; list of number of guesses

(define (master-mind-all-games candidates create-cand)
  (if (null? candidates)
      '()
      (cons (cadr (master-mind (car candidates) create-cand))
         (master-mind-all-games (cdr candidates) create-cand))))


(define candidates (create-candidates))

(let* ((candidates  (create-candidates))
       (len         (length candidates))
       (naive       (master-mind-all-games  candidates create-candidates))
       (naive-sum   (apply + naive))
       (naive-max   (apply max naive))                    
       (rnd         (master-mind-all-games  candidates create-candidates-rnd))
       (rnd-sum     (apply + rnd))
       (rnd-max     (apply max rnd))
       (opt         (master-mind-all-games  candidates create-candidates-opt))
       (opt-sum     (apply + opt))
       (opt-max     (apply max opt))
       (opt2        (master-mind-all-games  candidates create-candidates-opt2))
       (opt2-sum    (apply + opt2))
       (opt2-max    (apply max opt2))
              )
  (display "Naive guessing: ")
  (display naive-sum)
  (display " Average per game: ")
  (display (exact->inexact (/ naive-sum len)))
  (display " Maximum: ")
  (display naive-max)
  (newline)
  (display "Randomized guessing: ")
  (display rnd-sum)
  (display " Average per game: ")
  (display (exact->inexact (/ rnd-sum len)))
  (display " Maximum: ")
  (display rnd-max)
  (newline)
  (display "Optimized guessing: ")
  (display opt-sum)
  (display " Average per game: ")
  (display (exact->inexact (/ opt-sum len)))
  (display " Maximum: ")
  (display opt-max)
  (newline)
  (display "More Optimized guessing: ")
  (display opt2-sum)
  (display " Average per game: ")
  (display (exact->inexact (/ opt2-sum len)))
  (display " Maximum: ")
  (display opt2-max)
  (newline)
  )
 