#lang racket
(require "./ode/ode-library.rkt"
         "./sim/sim-library.rkt"
         "./plotter/plt-fnc.rkt"
         "./sim/sim-params.rkt"
         )
(provide 
         simulate-environment
         plot-environment
         make-environment
         make-env-ref-change
         plot-simdata
         )
;; -------------------------------------
;;  Rename the simulation parameters in
;;  term of enviromental parametrs
;; -------------------------------------
(define (make-env-prmtrs)
  (make-sim-prmtrs))
;; ------------------------

;; -------------------------------------
;;          Environment
;; -------------------------------------
;; An environment returns the name of the
;; generated ode file
;;-------------------------------------

        

;; -------------------------------------
;;         Make  Environment
;; -------------------------------------
;; An environment is a function that returns
;; a procedure to create an odefile when a
;; controller is given as argument.
;; T
;; The resultant odefile is the description 
;; in close-loop control-system in xxpaut
;; format.
;; -------------------------------------
;; This functions requires as arguments: 
;; system description, reference signal
;; and simulation parameters.
;; -------------------------------------
(define (build-environment system ref params)
  (lambda (ctrl)
    (environment->odefile system ctrl ref params)))
;; -------------------------------------
;; This function create the odefile asociated to
;; ctrl and enviiroment, run the simulation and
;; returns the data
;; -------------------------------------
(define (simulate-environment ctrl envr [erase? #t])
  (let ([odefile ((envr  'environment) ctrl)]
        ;[odefile (envr  ctrl)]
        )
    (simulate-ode odefile erase?)))

(define (plot-simdata data)
  (cond [(not (null? data))
         (plot-matrix (vector-map list->vector
                                  (list->vector data)))]
        ))
  

;; -------------------------------------
;; This function simulates an associate control
;; with the enviroment an plot the numeric data
;; -------------------------------------
(define (plot-environment ctrl envr [erase? #t])
  (let( [dat (simulate-environment ctrl envr erase?)])
    (plot-simdata dat)))

(define (name-environment envr)
  (let ([odefile (envr null)])
    ( extract-name odefile)))

(define (make-environment)
  (let (  
        [ref 1]
        [prms (make-env-prmtrs)]
        [syst null]
        [environment null]
        ) 
    
    (define (set-ref! refs)
      (set! ref refs))

    (define (set-name! name)
     (syst 'set-name! name))

   (define (set-sys! sys)
     (set! syst sys ))
   (define (build-environment!)
     (set! environment  (build-environment
                         syst
                         ref
                         prms)))
    
    (lambda (msg . args)
      (case msg
        [(environment) environment]
        [(ref) ref]
        [(system) syst]
        [(params-obj) prms]
        [(tf)  (prms 'TF)]
        [(t0)  (prms 'T0)]
        [(dt)  (prms 'DT)]
        [(name) (syst 'name)]
 
        [(set-tf!) (prms 'set-tf! (first args))]
        [(set-t0!) (prms 'set-t0! (first args))]
        [(set-dt!) (prms 'set-dt! (first args))]
        
        [(set-ref!) (apply set-ref! args)]
        [(set-sys!) (apply set-sys! args)]
        [(set-name!) (apply set-name! args)]
        [(update-environment!) (build-environment!)]
        ))))

(define (make-heavs change timeup timedown)
  (let* ([heavup
          (list '*
                change
                (list 'heav (list '- 't timeup)))]
         [heavdw
          (list '*
                (- change)
                (list 'heav (list  '- 't timedown)))]
         [heavs (list '+ heavup heavdw)]
         )
    heavs
    ))

(define (make-env-ref-change ref factor timesim t1 t2 [sign '+])
  (let* ([refp (* factor ref)]
         [timeup (* t1 timesim)]
         [timedw (* t2 timesim)]
         [heavs
          (if (eq? sign '+)
              (make-heavs refp timeup timedw)
              (make-heavs (- refp) timeup timedw))]
         )
    (list '+ ref heavs)))


;; -------------------
;;  DEBUGG  FUNCTIONS
;; --------------------------
 
;;; -------------------------------
;;; High level - Debug Functions
;;; -------------------------------

;; (require  "../plant/plant-library.rkt"
;;           "./sim/sim-params.rkt")
;; (define _plant (FirstOrder))
;; (define _ctrl-1 '(* 10 e))
;; (define _ctrl-0 '(* 5 e))

;; (define _dt 10e-3)
;; (define _t0 0.0)
;; (define _tf 1)

;; (define _ref 10)

;; (define _enviro (make-environment))
;; (_enviro 'set-ref! _ref)
;; (_enviro 'set-sys! _plant)
;; (_enviro 'set-dt! _dt)
;; (_enviro 'set-t0! _t0)
;; (_enviro 'set-tf! _tf)
;; (_enviro 'update-environment!)


;; (simulate-environment _ctrl-1 _enviro #f)
;; (plot-environment  _ctrl-1 _enviro)


