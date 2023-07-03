#lang racket
(require "./trees/trees.rkt"     )

(provide make-controller
         build-controller
         k-controller
         f-controller
         sigma-controller
         operator-controller
         numeric-random-value
         make-full-tree
         make-sigma-function
         make-f-function
         make-constant
         binary-node
         ) 


(define (numeric-random-value)
  (numeric-leaf-value)
  )
(define (make-f-function)
  (make-full-tree 3))

(define (make-sigma-function)
  (make-full-tree 3))

(define (make-constant)
  (numeric-leaf-value))

(define (operator-controller controller)
  (first controller))

(define (ktanhs-controller controller)
  (second controller))

(define (f-controller controller)
  (third controller))

(define (k-controller controller)
  (let ([ktanhs (ktanhs-controller controller)])
    (second ktanhs)))

(define (sigma-controller controller)
  (let ([ktanhs (ktanhs-controller controller)])
    (second (third ktanhs))))

(define (build-controller operator k sigma f [switch-op 'sign])
  (let*( [tanh-s (make-utree switch-op sigma)]
         [ktanh-s (make-btree '* k tanh-s)]
         )
    (make-btree operator ktanh-s f)))

(define (make-controller
         [switch-op 'sign]
         [operator (binary-node)])
  (let*( [f (make-f-function)]
         [sigma (make-sigma-function)]
         [k (* 50.0 (make-constant))]
         ;[operator (binary-node)]
         )
    (build-controller operator k sigma f switch-op)))



  
