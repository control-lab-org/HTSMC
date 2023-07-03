#lang racket
(require
 "./tree-ctrl/mutation-controller-htsmc.rkt"
 "./tree-ctrl/mutation-controller-htf.rkt"
 "./tree-ctrl/mutation-controller-ksigma.rkt"
 "./tree-ctrl/mutation-controller-integral.rkt"
 )
(provide make-ctrl
         make-controller-family
         mutate-ctrl
         crossover-ctrl
         replicate-ctrl
         set-ctrl-type!
         set-switching-op!
         )

(define ctrl-current-type 'htf)
(define switching-operator 'tanh)

(define (set-ctrl-type! type)
  (set! ctrl-current-type type))
 (define (set-switching-op! type)
  (set! switching-operator type))

(define (make-ctrl)  
  (cond
    [(equal? 'ksigma ctrl-current-type)
     (make-ksigma-controller switching-operator)]
    [(equal? 'htf ctrl-current-type)
     (make-htf-controller switching-operator)]
    [(equal? 'htsmc ctrl-current-type)
     (make-htsmc-controller switching-operator)]
    ;; [(equal? 'integral ctrl-current-type)
    ;;  (make-integral-controller switching-operator)]
    ))
  
  


(define (make-controller-family [n 5])
  (map (lambda (i)  (make-ctrl)) (range n))) 

(define (mutate-ctrl ctrl)
  (cond
    [(equal? 'ksigma ctrl-current-type)
     (mutate-ksigma ctrl switching-operator)   
     ]
    [(equal? 'htf ctrl-current-type)
     (mutate-htf  ctrl switching-operator)  
     ]
    [(equal? 'htsmc ctrl-current-type)
     (mutate-htsmc  ctrl switching-operator)]

   

     ))
 

(define (crossover-ctrl ctrl0 ctrl1 )

  (cond
    [(equal? 'ksigma ctrl-current-type)
     (crossover-ksigma ctrl0 ctrl0 switching-operator)   
     ]
    [(equal? 'htf ctrl-current-type)
     (crossover-htf  ctrl0 ctrl1 switching-operator)  
     ]
    [(equal? 'htsmc ctrl-current-type)
     (crossover-htsmc  ctrl0 ctrl1 switching-operator)
     ]
    ;; [(equal? 'integral ctrl-current-type)
    ;;  (crossover-htsmc  ctrl0 ctrl1 switching-operator)
    ;;  ]

    ))

(define (replicate-ctrl ctrl)
  ctrl
)

