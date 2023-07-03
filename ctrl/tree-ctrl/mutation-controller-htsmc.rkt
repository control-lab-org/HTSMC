#lang racket
(require "./mutation-controllers.rkt")
(require "./tree-controller-htsmc.rkt"     )
(require "./trees/tree-mutations-lib.rkt")

(provide replicate-htsmc
         crossover-htsmc
         mutate-htsmc
         make-htsmc-controller
        ; random-item
         members-list
         )

(define members-list (list 'k 's 'f))
(define switching-op-list (list 'tanh 'sign))
;(define switching-operator 'sign)

(define (replicate-htsmc htsmc-ctrl)
  htsmc-ctrl)

(define (make-htsmc-controller [switching-operator 'sign])
  (make-controller switching-operator))

(define (mutate-htsmc controller [switching-operator 'sign])
  (let* ([op1 (operator-controller controller)]
         [k (k-controller controller)]
         [s (sigma-controller controller)]
         [f (f-controller controller)]
         [member (random-item members-list)]
         [op (random-item (list op1 (binary-node)))])
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

(define (crossover-htsmc controller-1 controller-2 [switching-operator 'sign])
  (let* ([op1 (operator-controller controller-1)]
         [k1 (k-controller controller-1)]
         [s1 (sigma-controller controller-1)]
         [f1 (f-controller controller-1)]
         [op2 (operator-controller controller-2)]
         [k2 (k-controller controller-2)]
         [s2 (sigma-controller controller-2)]
         [f2 (f-controller controller-2)]
         [member (random-item members-list)]
         [op (random-item (list op1 op2))]
             )
    (define edited
      (cond
        [(equal? member 'k) (crossover-constants k1 k2)]
        [(equal? member 's) (crossover-trees s1 s2)]
        [(equal? member 'f) (crossover-trees f1 f2)]
        [else (error "vacio edited")])
      )
    edited
    (cond
      [(equal? member 'k)
       (build-controller op edited s1 f1 switching-operator)]
      [(equal? member 's)
       (build-controller op k1 edited f1 switching-operator)]
      [(equal? member 'f)
       (build-controller op k1 s1 edited switching-operator)]
      [else (error "vacio" member)]
      )))




