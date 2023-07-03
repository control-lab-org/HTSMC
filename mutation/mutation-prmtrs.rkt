#lang racket
(define (make-operations-prmtrs)
  (let ([mutation-prob  0.6]
        [replicate-prob 0.1]
        [crossover-prob 0.3])
    (lambda (msg . args)
          (case msg
            [(mutation-prob) mutation-prob]
            [(replicate-prob) replicate-prob]
            [(crossover-prob) crossover-prob]))))
(provide (all-defined-out))