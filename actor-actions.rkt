#lang racket

(require "engine.rkt"
         "struct+.rkt")

(provide actor-move actor-take)

(define (actor-move w actor-name dir-str)
  [define dest-i
    (hash-ref (deep-ref w '(focus adj)) dir-str (w 'index))]
  (teleport-thing w actor-name dest-i))

(define (actor-take w actor-name name)
  [define-values (thing v) (take-thing w name)]
  (insert-thing v actor-name thing))