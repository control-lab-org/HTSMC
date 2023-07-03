#lang racket
(provide sim-prms
         make-sim-prmtrs
         )

(define (make-sim-prmtrs)
  (let ([DT  100e-3]
        [T0  0.0]
        [TF  10]
        [BOUNDS 1000]
        [METHOD "euler"] 
        )

    (define (set-dt! new-dt)
      (set! DT new-dt))

    (define (set-t0! new-t0)
      (set! T0 new-t0))

    (define (set-tf! new-tf)
      (set! TF new-tf))

    (define (set-bounds! new-bounds)
      (set! BOUNDS new-bounds))
    
    (lambda (msg . args)
          (case msg
            [(DT) DT]
            [(T0) T0]
            [(TF) TF]
            [(METHOD) METHOD]
            [(BOUNDS) BOUNDS]
            [(set-dt!)              (apply set-dt! args) ]
            [(set-t0!)              (apply set-t0! args) ]
            [(set-tf!)              (apply set-tf! args) ]
            [(set-bounds!)          (apply set-bounds! args) ]
            
            ))))

(define sim-prms (make-sim-prmtrs))