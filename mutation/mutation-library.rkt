#lang racket
(require "../ctrl/ctrl-library.rkt")
(require "./mutation-prmtrs.rkt")

(define GApar (make-operations-prmtrs))

(provide   genetic-operations
          )

(define (make-individual fitness indv)
  (list fitness indv))
(define (fitness-individual indv)
  (first indv))
(define (controller-individual indv)
  (second indv))



(define (get-ntop population n-indvs)
  (let* ([top (take population n-indvs)])
     top))

(define (operate-indv indv best)
  (let ([selector (random)]
        [ctrl (controller-individual indv)]
        [best-ctrl (controller-individual best)]
        )
    
    (if (< selector (GApar 'replicate-prob))
        (replicate-ctrl  ctrl)
        (if (< selector  (+ (GApar 'replicate-prob) (GApar 'crossover-prob)))
            (crossover-ctrl  best-ctrl ctrl)
            (mutate-ctrl ctrl)
           ))))

(define (ctrls-elite individuals)
  (map controller-individual individuals))

(define (genetic-operation individuals best)
  (map
   (lambda (indv) (operate-indv indv best))
   individuals))

 (define (genetic-operations population)
   (let* ([sizepop (length population)]
          [top (inexact->exact
                (floor (* 0.1 sizepop)))]
          [elite  (get-ntop population top) ]    
          [elite-ctrls (ctrls-elite elite)]
          [best-indv (first population)]
         )
     (if (null? elite) 
         (genetic-operation population best-indv)
         (append elite-ctrls
                 (genetic-operation
                  (list-tail population top)
                  best-indv)))   
    ))




;; ;; (require "../evaluator/evaluator-library.rkt")
;; ;; (require "../report/report-library.rkt")
;; (require "../environment/environment-library.rkt") 
;; ;; (define evolution-times 3)


;;  (define family-ctrl (make-controller-family 10))

;; (define ep (evaluate family-ctrl _enviro))
;; (define best (first ep))
;; (define ind (third ep))
