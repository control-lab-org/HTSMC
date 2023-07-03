#lang racket
(require "./plant/plant-library.rkt"
         "./ctrl/ctrl-library.rkt"
         "./coupling/coupling-library.rkt"
         "./environment/sim/sim-params.rkt"
         "./environment/environment-library.rkt"
         )

(system "rm -r ./reports")
(system "rm -r ./odefiles")
(system "rm -r ./datfiles")

;; -----------------;; -----------------;; -----------------
;; -----------------;;       MODEL      ;; -----------------
;; -----------------;; -----------------;; -----------------
;; AVALIABLE MODELS:
;; (SimplePendulum) (BuckConverter) (BoostConverter)
;; (2realpoles)(FirstOrder)
;; Only take in consideration the parametes simulation
;; for the systems
;; TO MODIFY THE MODEL PARAMETERS
;; GO TO: ./PG230628a/plant/plant-library.rkt
;;;; ---------------------------------------------------------
 (define _plant (SimplePendulum))
;; ---------------------------------------------------------
;; -----------------;; -----------------;;-----------------
;; -----------------;;  SIM PARAMS      ;;-----------------
;; -----------------;; -----------------;;-----------------
(define _ref 1.56)
(define _t0   0.0)
(define _tf   10)
(define _dt   1e-3)
;; --------------------------------------------------
;; Positive delta of 0.2*_ref at the 0.5*_tf and recovers at _tf
;; the recovers is not showed
(define _ref_training
   (make-env-ref-change _ref 0.2 _tf 0.33 0.66 '+))
;; --------------------------------------------------
;; Positive Delta of 10% of _ref (0.1)  at the 0.333 of the tf and recover at 0.666 of the tf
(define _re+ (make-env-ref-change _ref  0.1 _tf 0.33 0.66 '+))
;; --------------------------------------------------
;; Negative Delta of -10% of _ref (0.1) the 0.333 of the tf and recover at 0.666 of the tf
(define _re- (make-env-ref-change _ref  0.1 _tf 0.33 0.66 '-))
;; ---------------;; ---------------------;; --------------
;; ---------------;; Environment Creation ;; --------------
;; ---------------;; ---------------------;; --------------
(define _enviro (make-environment))
(_enviro 'set-ref! _ref_training)
(_enviro 'set-sys! _plant)
(_enviro 'set-dt! _dt)
(_enviro 'set-t0! _t0)
(_enviro 'set-tf! _tf)
(_enviro 'update-environment!)
(define _name (_enviro 'name))
;; ---------------;; ---------------------;; --------------
;; ---------------;; Cost Function Weigths;; --------------
;; ---------------;; ---------------------;; --------------
(set!-norm2f  1.0)
(set!-itaef   1.0)
(set!-dspeedf 100.0)
;; -----------------;; -----------------;; -----------------
;; -----------------;;    Controllers   ;; -----------------
;; -----------------;;       types      ;; -----------------
;; -----------------;; -----------------;; -----------------
(define ctrl-type    (third   (list 'htsmc 'htf 'ksigma)))
(define switching-op (first   (list 'tanh  'sign  'integral)))
(set-ctrl-type!  ctrl-type )
(set-switching-op! switching-op)
;; -----------------;; -----------------;; -----------------
;; -----------------;;     Parameters   ;; -----------------
;; -----------------;; Genetic Algoritm ;; -----------------1
;; -----------------;; -----------------;; -----------------
(define family-size 400)
(define evolution-times 50)
;;  -----------------;; -----------------;; -----------------
;;  -----------------;;    FIRST FAMILY  ;; -----------------
;;  -----------------;; -----------------;; -----------------
(define family-ctrl (make-controller-family family-size))
family-ctrl
;; ;; ---------------;;---------------------;; --------------
;; ;; ---------------;;      EVOLUTION      ;; --------------
;; ;; ---------------;;---------------------;; --------------  
(define all-populations
  (evolution family-ctrl _enviro evolution-times))
;; ;; ---------------;;---------------------;; --------------
;; ;; ---------------;;      RESULTS        ;; --------------
;; ;; ---------------;;---------------------;; --------------  
(define lastpopulation (last all-populations))
(define best-indv (first lastpopulation))
(define best-lastctrl
  (controller-individual best-indv))
(define best-fitness (fitness-individual best-indv))
(_enviro 'set-name! (string-append _name "-b"))
(plot-environment best-lastctrl  _enviro #f)
;; --------------------------------------------------------
;; PLOT RESULTS
;; --------------------------------------------------------    
(define hsim (make-data-5best all-populations _enviro))
(_enviro 'set-ref! _re+)
(_enviro 'update-environment!)
(_enviro 'set-name!  "brup")
(define fitup (first (evaluate-controller  best-lastctrl  _enviro)))
(plot-environment best-lastctrl  _enviro #f) 

(_enviro 'set-ref! _re-)
(_enviro 'update-environment!)
(_enviro 'set-name! "brdw")
(define fitdown (first (evaluate-controller  best-lastctrl  _enviro)))
(plot-environment best-lastctrl  _enviro #f)

best-fitness
fitup
fitdown



    
