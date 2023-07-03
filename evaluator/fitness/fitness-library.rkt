#lang racket
(require "./dfnc.rkt"
          "../../environment/sim/sim-params.rkt"
          "../../environment/sim/sim-library.rkt"
         )
(provide
 set!-norm2f
 set!-itaef
 set!-dspeedf
 fitness
 make-simdata)


(define norm2-fact 5.0)
(define itae-fact 1.0)
(define dspeed-fact 100.0)


(define (set!-norm2f val)
  (set! norm2-fact val))

(define (set!-itaef val)
  (set! itae-fact val))

(define (set!-dspeedf val)
  (set! dspeed-fact val))
;; ---------------------------------------
;; Constant definitions
;; ---------------------------------------
(define bias-value 00.00)
(define foult-fitness-value  1000)

(define simpar null)
;; ---------------------------------------
;; Divergence speed function
;; ---------------------------------------
(define (divergence-speed signal)
  (let* (;[simpar sim-prms]
         [t0 (simpar 'T0)]
         [tf (simpar 'TF)]
         [tsim (* (vector-length (vector-ref signal 0))
                  (simpar 'DT))]
         [texp (- tf t0)]
         )
    (if (> tsim texp)
       0.0
       (- 1.0 (/ tsim texp)))))
    
;; ---------------------------------------
;; Computation of the bias
;; ---------------------------------------
(define (bias-func signal)   bias-value)
  
;; ---------------------------------------
;; linear combination of the proposed
;; fintess functions
;; ---------------------------------------
(define (fitness-fnc      signal
                          [w0 100.0]
                          [w1 1.0]
                          [w2 1.0]
                          [w3 0.0]
                          )
;(display (list w0 w1 w2 w3))
  (+
     (* w0 (divergence-speed  signal))
     (* w1 (norm-itaf signal))
     (* w2 (norm2     signal))
     (* w3 (bias-func signal))
     
     ))

(define (check-fitness fitness)
  (if (zero? fitness)
      foult-fitness-value
      fitness))
;; ---------------------------------------
;; This function compute the fitness of
;; a simulation result
;; ---------------------------------------
(define (compute-fitness simresult )
  (let* ([simdata (make-simdata simresult)]
         [ t   (simdata 't-vec )]
         [ e   (simdata 'e-vec)]
         [ k (list->vector (map (lambda (x) 10.0) (range
                                  (vector-length t))))]
         [ k-vect (make-dfnc t k)]
         [error-vctr (make-dfnc t e)]
         [fitness1   (fitness-fnc  error-vctr
                                   dspeed-fact
                                   itae-fact
                                   norm2-fact
                                   )]
         [ x1  (simdata 'x1-vec)]
         [x1-vctr (make-dfnc t x1)]
         [fitness2   (fitness-fnc
                      (dfnc- k-vect
                             x1-vctr)
                      100.0
                      10.2
                      0.2)]
         )
    (+ (check-fitness fitness1)
       (check-fitness fitness2)
       )
    ))
    


;; ---------------------------------------
;; Compute the fitness of the simdata file
;; ---------------------------------------
(define (fitness simdatares environment)
  (set! simpar (environment 'params-obj))
  (if (null? simdatares)
      foult-fitness-value
      (compute-fitness simdatares)))
     
        

;;; --------------------
;;;     Debugg Code
;;; --------------------
;;; array (t,x0,x1,r,y ,e,u)
;

;;-------------------------
;; Value exptected: fce-e
;; divergence: 0.0  wx10
;; itaef: 91.10     wx10
;; snorm-2: 4571.43 wx10
;;
;; TOTAL: 46625.53718670652
;;-------------------------

;;  (define array-data
;;    '(
;;      (0 0.1 0 5 0.1 4.9000001 49.994453)
;;      (0.5 1.1 19.413538 5 1.1 3.9000001 49.959042)
;;      (1 2.0999999 30.747015 5 2.0999999 2.9000001 49.698158)
;;      (1.5 3.0999999 36.87785 5 3.0999999 1.9 47.811874)
;;      (2 4.0999999 37.842999 5 4.0999999 0.89999998 35.814892 )
;;      ))

;;   (define test-data (make-simdata array-data))
;; ;;  (define t (test-data 't-vec))
;; ;;  (define y (test-data 'y-vec))
;;   (define e (test-data 'e-vec))
;;   (define x1 (test-data 'x1-vec))
;;  (define fnc-y (make-dfnc t y))
;;  (define fnc-e (make-dfnc t e))
;;  (divergence-speed fnc-e) 
;;  (norm-itaf fnc-e)
;;  (square-norm2 fnc-e)
;; (fitness-fnc fnc-e)
;; ------------------------------

             
;; (define _t0   0.0) (define _tf   0.05) (define _dt   2000e-6)
;; (define datasim
;; '((0 0 10 0 10)
;;   (0.0020000001 0.19761908 10 0.19761908 9.8023806)
;;   (0.0040000002 0.39055178 10 0.39055178 9.6094484)
;;   (0.0060000001 0.57890922 10 0.57890922 9.4210911)
;;   (0.0080000004 0.76279986 10 0.76279986 9.2371998)
;;   (0.0099999998 0.9423297 10 0.9423297 9.0576706)
;;   (0.012 1.1176021 10 1.1176021 8.8823977)
;;   (0.014 1.288718 10 1.288718 8.7112818)
;;   (0.016000001 1.4557761 10 1.4557761 8.5442238)
;;   (0.017999999 1.6188725 10 1.6188725 8.3811274)
;;   (0.02 1.7781012 10 1.7781012 8.221899)
;;   (0.022 1.9335538 10 1.9335538 8.0664463)
;;   (0.024 2.08532 10 2.08532 7.91468)
;;   (0.026000001 2.2334874 10 2.2334874 7.7665129)
;;   (0.028000001 2.3781407 10 2.3781407 7.6218591)
;;   (0.029999999 2.5193639 10 2.5193639 7.4806361)
;;   (0.032000002 2.657238 10 2.657238 7.342762)
;;   (0.034000002 2.7918427 10 2.7918427 7.2081575)
;;   (0.035999998 2.9232552 10 2.9232552 7.076745)
;;   (0.037999999 3.0515513 10 3.0515513 6.9484487)
;;   (0.039999999 3.176805 10 3.176805 6.823195)
;;   (0.041999999 3.2990885 10 3.2990885 6.7009115)
;;   (0.044 3.4184721 10 3.4184721 6.5815282)
;;   (0.046 3.5350244 10 3.5350244 6.4649754)
;;   (0.048 3.648813 10 3.648813 6.3511872)
;;   (0.050000001 3.759903 10 3.759903 6.240097)))

;; (define test-data2 (make-simdata datasim))
;; (define t2 (test-data2 't-vec))
;; (define y2 (test-data2 'y-vec))
;; (define e2 (test-data2 'e-vec))
;; (define fnc-y2 (make-dfnc t2 y2))
;; (define fnc-e2 (make-dfnc t2 e2))

;; (divergence-speed fnc-e2) 
;; (norm-itaf fnc-e2)
;; (square-norm2 fnc-e2)
;; (fitness-fnc fnc-e2)
