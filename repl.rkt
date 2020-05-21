#lang racket

(require "engine.rkt" "actor-actions.rkt"
         data/pvector)

(define ball (Thing "ball" "a small rubber ball"))
(define box (Bag "box" "a cardboard box" (hash)))
(define player (Bag "player" "Yourself" (hash)))

(define world
  (World
   0
   (Room "a" "It's a." (hash "s" 2 "n" 1) (hash "box" box
                                                "player" player))
   (pvector
    null
    (Room "b" "This is b." (hash "s" 0 "n" 2) (hash))
    (Room "c" "Now c." (hash "s" 1 "n" 0) (hash)))))

(define (trim-whitespace str)
  (first (regexp-match #px"\\S.*\\S|\\S|$" str)))

(define (clean-split pat str)
  (filter
   (λ(s) (not (equal? "" s)))
   (map trim-whitespace (regexp-split pat str))))

(define (repl w)
  (displayln ((w 'focus) 'desc))
  [define ls (clean-split " " (read-line))]
  (case (first ls)
    [("move") (repl (actor-move w "player" (second ls)))]))