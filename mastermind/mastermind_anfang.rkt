#lang racket

; --Mastermind--
; farben werden als zahlen 1-6 kodiert
; feedback als 2er-Liste von Zahlen
; 1. element: korrekkte farbe an korrekter poisition
; 2. element: korrekte farbe, aber falsche position

(define (mastermind solution)
    (print_init l)
    (solve-mm solution )
)

; bewerte guess relativ zu soln
(mm-eval guess soln)
    ; zähle werte an gleichen positionen
    (exact-matches l1 l2)
    ; zähle, wie viele elementen aus l1 in l2 vorkommen
    (count-occ l1 l2)
        ; gib lst ohne e zurück
        (delete-element e lst)
        (print_guess guess lst)

; generiere alle codes für pins stifte aus colours Farben
(make-guesses pins colours)
    ; ergänze alle tuple um alle farben
    (add-to-tuples tuples colours)
        ; ergänze ein tuple um alle farben
        (add-to-tuple tuple colours)

(define (solve-mm solution candidates)
    (filter-compatible candidates guess ev)
        (is-compatible candidate guess ev)
)

(define (print_init code)
    (display "Game initialized. Secret Code: ")
    (display code)
    (newline)
)

(define (print_guess guess eval)
    (display "Guess: ")
    (display guess)
    (display " -> ")
    (display eval)
    (newline)
)

(mastermind '(4 2 2 1))