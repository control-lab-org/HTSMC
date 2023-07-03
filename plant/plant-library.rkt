#lang racket
(require "./rdynsys.rkt")
(provide (all-defined-out))
;; TODO: test-plantlib.rkt ... done!
;; This library defines models of
;; different dynamic systems ready
;; to use. If you need a specific model
;; can use "MyModel" template to define it.

;; The model name length must be less than 10
;; characters
;; =============================================
;;    mymodel
;;   write your model code here, use the template
;; =============================================


(define (MyModel)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          ;(x1p)
          ;(x2p)
          ))
  (plant 'set-out-expr! '( (x1) (x2) ))
  (plant 'set-state-symb! '(x1 x2) )
  (plant 'set-prmtr-symb! '() )
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(-10.0))
  (plant 'set-init-cond! '(0.5 -0.2) )
  (plant 'set-name! "mymodel")

  )



;; =============================================
;;    First Order Model
;;         x1p= a*x1 + u           
;; =============================================
(define (FirstOrder)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          (+ (* a x1)  (* b u))
          
          ))
  (plant 'set-out-expr! '( (x1)  ))
  (plant 'set-state-symb! '(x1 ) )
  (plant 'set-prmtr-symb! '(a b) )
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(2.0 1.0))
  (plant 'set-init-cond! '(0.0) )
  (plant 'set-name! "Fo")
  plant)


;; ============================================= 
;;  Second Order Model: 2 real-poles
;;     x1p= x2
;;     x2p= -(a*c)x1 - (a+c)x2 + b*u
;;          TF= b/(s+a)(s+c)
;; =============================================
(define (2realpoles)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '((x2)
          (+ (- (- (* (* a b) x1)) (* (+ a b) x2))
             (* c u))
          ))
  (plant 'set-out-expr! '( (x1) (x2) ))
  (plant 'set-state-symb! '(x1 x2))
  (plant 'set-prmtr-symb! '(a b c))
  (plant 'set-in-symb! '(u))
  (plant 'set-init-cond! '(0.1 0.0))
  (plant 'set-prmtr-val! '(10.0 -2.0 1.0))
  (plant 'set-name! "2RP")
  plant)

;; ============================================= 
;; Simple Pendulum Model
;;         x1p= x2
;;         x2p= -(g/l)*sin(x1) - b*x2 + u/(m*l*l)
;; =============================================
(define (SimplePendulum)
  (define plant     (make-rdynsys))
  (plant  'set-state-expr!
        '((x2)
          (+ (- (- (* (/ g l) (sin x1)))
                (* (* b m) x2))
             (/ u (* m (* l l))))
          ))
  (plant 'set-out-expr! '( (x1) (x2) ))
  (plant 'set-state-symb! '(x1 x2))
  (plant 'set-prmtr-symb! '(m g l b))
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(0.5 9.81 1.0 1.0))
  (plant 'set-init-cond! '(0.1 0.0))
  (plant 'set-name! "SP")
  plant )



;; ============================================= 
;; Buck Converter 
;;         x1p= x1/C - x2/RC
;;         x2p= -x2/L + (Vin/L)*u 
;; =============================================
(define (BuckConverter)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          (+ (- (/  x2 L)) (* (/ Vin L) u))
          (- (/ x1 C) (/ x2 (* R C)) )
          ))
  (plant 'set-out-expr! '( (x1) (x2) ))
  (plant 'set-state-symb! '(x1 x2))
  (plant 'set-prmtr-symb! '(R L C Vin))
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(6.0 500e-6 33e-6 24.0))
  (plant 'set-init-cond! '(0.0 0.0))
  (plant 'set-name! "Bck")
  plant)


;; =============================================
;; Boost Converter
;;         x1p= Vin/L  - (x2/L)*u
;;         x2p= (x1/C)*u - x2/RC 
;; =============================================
;; (define (BoostConverter)
;;   (define plant (make-rdynsys))
;;   (plant  'set-state-expr!
;;         '(
;;           (-  (/ Vin L) (* (/  x2 L) u))
;;           (-  (* (/ x1 C) u)  (/ x2 (* R C)) )
;;           ))
;;   (plant 'set-out-expr! '( (x2) (x1) ))
;;   (plant 'set-state-symb! '(x1 x2))
;;   (plant 'set-prmtr-symb! '(R L C Vin))
;;   (plant 'set-in-symb! '(u))
;;   (plant 'set-prmtr-val! '(15.0 500e-6 47e-6 12.0))
;;   (plant 'set-init-cond! '(0.0 0.0))
;;   (plant 'set-name! "Bst")
;;   plant)

;;
;; https://ieeexplore.ieee.org/stamp/stamp.jsp?tp=&arnumber=9254912
(define (BoostConverter)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          (-  (/ Vin L) (* (/  x2 L) u))
          (-  (* (/ x1 C) u)  (/ x2 (* R C)) )
          ))
  (plant 'set-out-expr! '( (x2) (x1) ))
  (plant 'set-state-symb! '(x1 x2))
  (plant 'set-prmtr-symb! '(R L C Vin))
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(15.0 500e-6 47e-6 5.0))
  (plant 'set-init-cond! '(0.0 0.0))
  (plant 'set-name! "Bst")
  plant)

;; =============================================
;; Magnetic Levitation 
;;         x1p= x2
;;         x2p= (k/m)*(x3/x1)^2 + g
;;         x3p= (1/L)*u -(R/L)*x3
;; k [Nm2/A2]

;; https://www.lancaster.ac.uk/staff/ruanw/papers/jp2014_wj.pdf
;; Range 0-16mm
;; =============================================
(define (MagneticLevitator)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          (x2)
          (+  (* (/ k m) (/  x3 x1) (/  x3 x1)) g)
          (-  (* (/ 1 L) u) (* (/ R L) x3))
          ))
  (plant 'set-out-expr! '( (x1) (x2) (x3) ))
  (plant 'set-state-symb! '(x1 x2 x3))
  (plant 'set-prmtr-symb! '(R L m g k))
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(13.8 11e-3 0.022 9.81  23.142e-3))
  (plant 'set-init-cond! '(-5e-3 0.0 0.0))
  (plant 'set-name! "ML")
  plant)

;; =============================================
;; Magnetic Levitation Lineazate
;;         x1p= x2
;;         x2p= (k/m)*(L*x3) + g
;;         x3p= (1/L)*u -(R/L)*x3
;; k [Nm2/A2]
;; =============================================
(define (MagneticLevitatorLin)
  (define plant (make-rdynsys))
  (plant  'set-state-expr!
        '(
          (x2)
          (+  (* (/ k m) (*  L x3))  g)
          (-  (* (/ 1 L) u) (* (/ R L) x3))
          ))
  (plant 'set-out-expr! '( (x1) (x2) (x3) ))
  (plant 'set-state-symb! '(x1 x2 x3))
  (plant 'set-prmtr-symb! '(R L m g k))
  (plant 'set-in-symb! '(u))
  (plant 'set-prmtr-val! '(13.8 11e-3 0.022 9.81  23.142e-3))
  (plant 'set-init-cond! '(-0.005 0.0 0.0))
  (plant 'set-name! "MLL")
  plant)


(define (make-plant name)
  (name))


;;; | ----- Debug Code -------|
;(define testmodel (SimplePendulum))
;(testmodel 'state-expr)
;(testmodel 'out-expr)
