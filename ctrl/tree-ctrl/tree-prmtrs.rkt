#lang racket
(provide (all-defined-out))
;;-----------------------------------------------
;; Parameters names are modified in the M20230403
;; version
;;-----------------------------------------------
;;Changes:
;; - "sensors" was changed as "symbolics"
;;-----------------------------------------------
(define (make-tree-prmtrs)
  (let ([states '()]
        [inputs  '(e  R L C)]
        [leaf/node-probability 0.9] 
        [unary/binary-probability  0.5] 
        [symbolic/number-probability 0.1] 
        [max-range 5.0]
        [min-range -0.5]
        [min-depth 1]
        [max-depth 5]
        [unary-nodes '( - )]
        [binary-nodes '(+ * - /)]
        )
        (lambda (msg . args)
          (case msg
            [(symbolics) (append states inputs)]
            [(leaf/node-probability) leaf/node-probability]
            [(symbolic/number-probability) symbolic/number-probability]
            [(max-range) max-range]
            [(min-range) min-range]
            [(min-depth) min-depth]
            [(max-depth) max-depth]
            [(unary/binary-probability) unary/binary-probability]
            [(unary-nodes) unary-nodes]
            [(binary-nodes) binary-nodes]
            [(nodes) (append unary-nodes binary-nodes)]))))
