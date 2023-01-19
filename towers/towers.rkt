;;; Piles of disks. Smaller disks are in front

(define (make-pile n)
  (if (= n 0)
      '()
      (append (make-pile (- n 1)) 
              (list n))))

;;; A single tower is a list, with Name/Number in car,
;;; and a pile (list of disk) in cadr. A tower configuration
;;; is a list of three towers with names A, B, and C

(define (make-initial-towers n)
  (list (list 'A (make-pile n))
        (list 'B '())
        (list 'C '())))

;;; Return the pile

(define (pile tower)
  (cadr tower))

;;; Return the name

(define (name tower)
  (car tower))

;;; Print a tower

(define (print-tower tower)
  (display (name tower))
  (display ": ")
  (display (pile tower))
  (newline))

;;; Print a tower config

(define (print-towers towers)
  (let ((tower-a (assoc 'A towers))
        (tower-b (assoc 'B towers))
        (tower-c (assoc 'C towers)))
    (print-tower tower-a)
    (print-tower tower-b)
    (print-tower tower-c)))


;;; Pretty printing

(define (pretty-print towers n)
  (if (> n 10)
      (print-towers towers)
      (display "Not now")))

;;; Is the tower empty?

(define (empty? tower)
  (null? (pile tower)))

;;; Return the top of the tower

(define (top-tower tower)
  (cond ((empty? tower)
         (display "Error: Empty tower (top) ")
         (display (name tower))
         (newline)
         (exit #t))
        (else 
         (car (pile tower)))))

;;; Return the tower without the top

(define (pop-tower tower)
  (cond ((empty? tower)
         (display "Error: Empty tower (pop) ")
         (display (name tower))
         (newline)
         (exit #t))
        (else 
         (list (name tower) (cdr (pile tower))))))
         
;;; Add a disk to the tower

(define (add-to-tower tower disk)
  (cond ((and (not (empty? tower))
             (< (top-tower tower) disk))
         (display "Error: Illegal move ")
         (newline))
        (else
         (list (name tower) (cons disk (pile tower))))))
        
;;; Move a disk from the first to the second tower in config

(define (move-disk towers)
  (let* ((from (car towers))
         (to   (cadr towers))
         (park (caddr towers))
         (disk (top-tower from)))
    (print-towers towers)
    (display "Moving disk ")
    (display disk)
    (display " from ")
    (display (name from)) 
    (display " to ")
    (display (name to))
    (newline)
    (list (pop-tower from) (add-to-tower to disk) park)))


 ;;; Move a tower from first to second tower. Return triple from, to, park 
(define (move-tower towers size)
  (if (= size 1)
      (move-disk towers)
      (let* ((from  (car towers))
             (to    (cadr towers))
             (park  (caddr towers))
             (step1 (move-tower (list from park to) (- size 1)))
             (from (car step1))
             (to   (caddr step1))
             (park (cadr step1))
             (step2    (move-disk (list from to park)))
             (from (car step2))
             (to   (cadr step2))
             (park (caddr step2))
             (step3 (move-tower (list park to from) (- size 1)))
             (from (caddr step3))
             (to   (cadr step3))
             (park (car step3)))
        (list from to park))))


;;; Comand line handling, initialization, print result

(define size
  (begin
    (display "Bitte Hoehe der Tuerme eingeben")
    (read)
        ))
      
(define config (make-initial-towers size))

(print-towers (move-tower config size))