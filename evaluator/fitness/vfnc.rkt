#lang racket

(define vith vector-ref)
(define (vfirst v) (vith v 0))
(define (vsecond v) (vith v 1))
(define (vthird v) (vith v 2))
(define (vlast v) (vith v (sub1 (vector-length v))) )


(define (v+ v1 v2)
  (vector-map + v1 v2))

(define (v- v1 v2)
  (vector-map - v1 v2))

(define (v.* v1 v2)
  (vector-map * v1 v2))

(define (v-neg v)
  (vector-map - v))

(define (v* v a)
  (vector-map (lambda (num) (* a num)) v))

(define (v-sum-elmnts v)
  (for/sum ([i v]) i))

(define (v. v1 v2)
  (v-sum-elmnts  (v.* v1 v2)))

(define (v-mag v)
  (sqrt (v. v v)))


;; ;; test
; (define v1 (vector 1 2 3 4 5))
; (define v2 (v* v1 0.3))
;; (define v3 (v* v1 0.7))
;; (define v4 (v+ v2 v3))
;; (define v5 (v- v3 v2))
;; (define v4.v5 (v. v4 v5))
;; (define mag-v1 (v-mag v1))
;(v.* v1 v1)
;; v1
;; v2
;; v3
;; v4
;; v5
;; v4.v5  ;; 22.0
;; mag-v1 ;; 7.416198...

;; export all defined function
(provide (all-defined-out))

