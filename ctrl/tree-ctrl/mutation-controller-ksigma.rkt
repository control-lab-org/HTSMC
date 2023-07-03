#lang racket
(require "./mutation-controller-htsmc.rkt")
(require "./tree-controller-htsmc.rkt")
(require "./mutation-controllers.rkt")
;(require "./trees/trees.rkt"     )

(provide replicate-ksigma
         crossover-ksigma
         mutate-ksigma
         make-ksigma-controller
         )

(define fix-operator '+)
(define (make-ksigma-controller [switching-operator 'sign])
  (let*( [f 0]
         [sigma (make-sigma-function)]
         [k (make-constant)]
         )
    (build-controller fix-operator k sigma f switching-operator)))


(define (replicate-ksigma ksigma-ctrl)
  ksigma-ctrl
  )

(define (mutate-ksigma controller [switching-operator 'sign])
  (let* ([op (operator-controller controller)]
         [f (f-controller controller)]
         [k (k-controller controller)]
         [s (sigma-controller controller)]
         [member (random-item (remove 'f members-list)) ]
         )
    (define edited
      (cond
        [(equal? member 'k) (mutate-constant k)]
        [(equal? member 's) (mutate-tree s)]
        )
      )
    
    (cond
      [(equal? member 'k)
       (build-controller op edited s f switching-operator)]
      [(equal? member 's)
       (build-controller op k edited f switching-operator)]      
      )
    ))

(define (crossover-ksigma controller-1 controller-2 [switching-operator 'sign])
  (let* ([op1 (operator-controller controller-1)]
         [f1 (f-controller controller-1)]
         [k1 (k-controller controller-1)]
         [s1 (sigma-controller controller-1)]
         [k2 (k-controller controller-2)]
         [s2 (sigma-controller controller-2)]
         [member (random-item (remove 'f members-list))]
         )
    (define edited
      (cond
        [(equal? member 'k) (crossover-constants k1 k2)]
        [(equal? member 's) (crossover-trees s1 s2)]
        [else (error "vacio edited")])
      )
    ;edited
    (cond
      [(equal? member 'k)
       (build-controller op1 edited s1 f1 switching-operator)]
      [(equal? member 's)
      (build-controller op1 k1 edited f1 switching-operator)]
      [else (error "vacio" member)]
      )))
    




