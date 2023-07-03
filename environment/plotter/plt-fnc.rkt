#lang racket

;(require "auxfnc.rkt")
(require plot)    ; used in proc plot matrix
;; (plot-new-window? #t)

;; creates a new matrix with column 0 and n of the original
(define (take-0-and-n matrix n)
  (vector-map
   (λ (vec) (vector (vector-ref vec 0)
                         (vector-ref vec n)))
   matrix))

(define (take-0-and-nl matrix n)
  (map
   (λ (vec) (list (list-ref vec 0)
                         (list-ref vec n)))
   matrix))

;; plot cols 1, 2 ... n vs col 0 of a matrix
(define (render2d matrix)
  (let* ([num-of-col (vector-length (vector-ref matrix 0))]
         [plts (map
                (lambda (n) (take-0-and-n matrix n))
                (range 1 num-of-col))])
    (map lines plts)))


(define (render2dl matrix)
  (let* ([num-of-col (length (list-ref matrix 0))]
         [plts (map
                (lambda (n) (take-0-and-nl matrix n))
                (range 1 num-of-col))])
    (map lines plts)))

(define (plot-matrix matrix)
   (plot (render2d matrix)))

;; export all defined function
(provide (all-defined-out))

