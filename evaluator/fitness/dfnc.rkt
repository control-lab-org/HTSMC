#lang racket
(require "./vfnc.rkt")

;; (define (make-kvect k size)
;;   (let ([k-list (map (lambda (x) k) (range size))]
;;         k-list)))

(define (make-dfnc x fx)
  (vector x fx))

(define (x-dfnc dfnc)
  (vith dfnc 0))

(define (fx-dfnc dfnc)
  (vith dfnc 1))

;; length of dfnc 
(define (dfnc-len dfnc)
  (let ([x (x-dfnc dfnc)]
        [fx (fx-dfnc dfnc)])
    (vector-length x)))


;; dfnc add f1(x) + f2(x)
(define (dfnc+ dfnc1 dfnc2)
  (make-dfnc (x-dfnc dfnc1) (v+ (fx-dfnc dfnc1) (fx-dfnc dfnc2))))

;; dfnc sub: f1(x) - f2(x) 
(define (dfnc- dfnc1 dfnc2)
  (make-dfnc (x-dfnc dfnc1) (v- (fx-dfnc dfnc1) (fx-dfnc dfnc2))))

;; dfnc element by element product: f1(x) * f2(x) 
(define (dfnc.* dfnc1 dfnc2)
  (make-dfnc (x-dfnc dfnc1) (v.* (fx-dfnc dfnc1) (fx-dfnc dfnc2))))

;; dfnc negative: -f(x)
(define (dfnc-neg dfnc)
  (make-dfnc (x-dfnc dfnc) (v-neg (fx-dfnc dfnc))))

;; scalar product: a(f(x))
(define (dfnc* dfnc a)
  (make-dfnc (x-dfnc dfnc) (v* (fx-dfnc dfnc) a)))

;; operation over a dfnc:  op(f(x))
(define (dfnc-op dfnc op)
  (make-dfnc (x-dfnc dfnc) (vector-map op (fx-dfnc dfnc))))

;; abs(f(x))
(define (dfnc-abs dfnc)
  (dfnc-op dfnc abs))

;; sqr(f(x))
(define (dfnc-sqr dfnc)
  (dfnc-op dfnc sqr))

;; trapezoidal integration method
(define (dfnc-int dfnc)
  (let ([x (x-dfnc dfnc)]
        [fx (fx-dfnc dfnc)])
    (* (-  (vsecond x ) (vfirst x)) ;; delta_x
       (- (v-sum-elmnts fx)
          (/ (+ (vfirst fx) (vlast fx)) 2.0))) )
  )

;; norm2:  sqrt( int( abs(f(x))^2 ) )
(define (norm2 dfnc)
  (if (> (dfnc-len dfnc) 1)
      (sqrt (dfnc-int (dfnc-sqr (dfnc-abs dfnc))))
      1000
      ))

;; NORM: norm2^2:  int( abs(f(x))^2 ) 
(define (square-norm2 dfnc)
  (if (> (dfnc-len dfnc) 1)
       (dfnc-int (dfnc-sqr (dfnc-abs dfnc)))
      1000
      ))

;; Integral Time Absolute Function
(define (norm-itaf dfnc)
  (if (> (dfnc-len dfnc) 1)
      (let ([timev (make-dfnc (x-dfnc dfnc) (x-dfnc dfnc))])
        (dfnc-int (dfnc.* timev (dfnc-abs dfnc))))
      1000
      ))

(define (norm-i dfnc)
  (if (> (dfnc-len dfnc) 1)
      (dfnc-int (dfnc-abs dfnc))
      1000
      ))



;; auxiliary functions

;; ;;===== interval and domain   =====  
 (define (make-interval ini end delta)
  (list ini end delta))

 (define (ini-interval interval)
  (list-ref interval 0))

 (define (end-interval interval)
  (list-ref interval 1))

 (define (delta-interval interval)
  (list-ref interval 2))

 (define (make-domain interval)
   (let ([ini (ini-interval interval)]
         [end (end-interval interval)]
         [dx (delta-interval interval)])
     (let ([n-muestras (add1 (exact-floor (/ (- end ini) dx)))])
       (build-vector n-muestras (lambda (n) (+ ini (* n dx)))))))

;; from normal func to discrete func
 (define (fnc->dfnc fnc domain)
   (make-dfnc domain (vector-map fnc domain)))

 (define (step x)
   (if (< x 0) 0.0 1.0) )

 (define (idnty x) x)


 ; test
; (define dm1 (make-domain (list -1 pi 0.5)))
; (define sin1 (fnc->dfnc sin dm1))
; (define c1 (fnc->dfnc step dm1))
; (define c2 (dfnc* c1 2.0))
; (define f1 (dfnc+ c2 sin1))
; (define r1 (dfnc* (fnc->dfnc idnty dm1) 1.5))
; (define ic1 (dfnc-int c1))
; (define ic2 (dfnc-int c2))
; (define isin1 (dfnc-int sin1))
; (define isin2 (dfnc-int (dfnc-abs sin1)))
; (define isin3 (dfnc-int (dfnc-sqr (dfnc-abs sin1))))
;(define n1 (norm2 c1))
; (define n2 (norm2 (dfnc- c2 c1)))
; (define n3 (norm2  sin1))
; (define n4 (norm2 (dfnc-neg sin1)))
; (define n5 (norm2 (dfnc- f1 c2)))

;; export all defined function
;(norm-itae c1)
(provide (all-defined-out))

