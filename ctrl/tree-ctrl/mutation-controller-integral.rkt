#lang racket

;(require "./tree-controller-integral.rkt")
(require "./mutation-controller-htsmc.rkt")
(require "./tree-controller-htsmc.rkt")

(provide replicate-icontroller
         crossover-icontroller
         mutate-icontroller
         make-integral-controller
         )



(define (make-integral-controller [operator 'integral])
  (make-controller operator))

(define (mutate-icontroller controller [operator 'integral])
  (mutate-htsmc controller operator))

(define (crossover-icontroller controller-1 controller-2 [operator 'integral])
(crossover-htsmc  controller-1 controller-2 operator))

(define (replicate-icontroller i-ctrl)
  i-ctrl)

;; --------------------------------------------------
;;  TEST CODE
;; --------------------------------------------------

;; (define ci-1 (make-integral-controller))
;; (define ci-2 (make-integral-controller))

;; (define c1-m  (mutate-icontroller (mutate-icontroller (mutate-icontroller ci-1))))

;; (define c2-m  (mutate-icontroller (mutate-icontroller (mutate-icontroller ci-2))))

;; (define ci-cross (crossover-icontroller ci-1 ci-2))

;; (crossover-icontroller (crossover-icontroller c1-m c2-m) ci-cross) 


    




