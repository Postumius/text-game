#lang racket

(require "struct+.rkt"
         "helper-macros.rkt"
         data/pvector
         data/collection)

(provide take-thing put-thing teleport-thing insert-thing
         remove-thing
         Room World Thing Bag)

(struct+ Room (name desc adj cont))

(struct+ World (index focus rooms))

(struct+ Thing (name desc))
(define ball (Thing "ball" "a small rubber ball"))

(struct+ Bag (name desc cont))
(define box (Bag "box" "a cardboard box" (hash)))

(define world
  (World
   0
   (Room "a" "It's a." (hash "s" 2 "n" 1) (hash "box" box))
   (pvector
    null
    (Room "b" "This is b." (hash "s" 0 "n" 2) (hash))
    (Room "c" "Now c." (hash "s" 1 "n" 0) (hash)))))

(define (switch-focus w i)
  (struct-match-copy
   World w
   [index i]
   [focus (nth rooms i)]
   [rooms (set-nth rooms index focus)]))

(define (upd-focus-cont w updater)
  ((deep-upd '(focus cont) updater) w))

(define ((cntnr-upd name updater) cont)
  (hash-update cont name
               (deep-upd '(cont)
                         updater)))

(define (take-thing w name)
  (values
   (hash-ref ((w 'focus) 'cont) name)
   (upd-focus-cont w (curryr hash-remove name))))

(define (put-thing w thing)
  (upd-focus-cont w (curryr hash-set (thing 'name) thing)))

(define (teleport-thing w name dest-i)
  (define-values (thing v) (take-thing w name))
  (put-thing (switch-focus v dest-i) thing))

(define (bag-cont-upd w bag-name updater)
  ((deep-upd '(focus cont)
             (curryr hash-update bag-name
                     (deep-upd '(cont)
                               updater)))
   w))

(define (insert-thing w bag-name thing)
  (bag-cont-upd
   w bag-name (curryr hash-set
                      (thing 'name) thing)))

(define (remove-thing w bag-name name)
  (values
   (((hash-ref ((w 'focus) 'cont) bag-name) 'cont) name)
   (bag-cont-upd
    w bag-name (curryr hash-remove name))))







