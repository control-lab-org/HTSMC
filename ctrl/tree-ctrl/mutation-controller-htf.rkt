#lang racket
(require "./mutation-controller-htsmc.rkt")
(require "./mutation-controllers.rkt")
(require "./tree-controller-htsmc.rkt")
(require "./trees/trees.rkt"     )
(provide replicate-htf
         crossover-htf
         mutate-htf
         make-htf-controller
         
         )
(define fix-operator '*)

(define (make-htf-controller [switching-operator 'sign])
  (make-controller switching-operator fix-operator))

(define (replicate-htf htf)
  htf)

(define (mutate-htf controller [switching-operator 'sign])
   (let* ([op (operator-controller controller)]
          [k (k-controller controller)]
          [s (sigma-controller controller)]
          [f (f-controller controller)]
          [member (random-item members-list)]
          )
     (define edited
       (cond
         [(equal? member 'k) (mutate-constant k)]
         [(equal? member 's) (mutate-tree s)]
         [(equal? member 'f) (mutate-tree f)]))
     edited
     (cond
       [(equal? member 'k)
        (build-controller op edited s f switching-operator)]
       [(equal? member 's)
        (build-controller op k edited f switching-operator)]
       [(equal? member 'f)
        (build-controller op k s edited switching-operator)])
     ))
(define (crossover-htf htf-1 htf-2 [switching-operator 'sign])
  (crossover-htsmc htf-1 htf-2   switching-operator))

