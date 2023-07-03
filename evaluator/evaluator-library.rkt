#lang racket
(require "./fitness/fitness-library.rkt")
(require "../environment/environment-library.rkt")

(provide  sort-agents
          ctrls<-agents 
          fitness<-agents
          dat<-agents

          fitness-agent
          ctrl-agent 
          dat-agent 
          
          evaluate-controllers
          evaluate-controller
          fitplot-controller
          
          evaluate
          set!-norm2f
          set!-itaef
          set!-dspeedf
 )
;; --------------------------------
;;             Agent
;; --------------------------------
;; An agent is an evaluated controller
;; where is capsulated in a list the
;; fitness value, control-law and
;; simulation data.
;; --------------------------------
;; Constructor and selectors
;; --------------------------------
(define (make-agent fitness ctrl mdata)
  (list fitness ctrl mdata))
;; --------------------------------
(define (fitness-agent agent)
  (first agent))

(define (ctrl-agent agent)
  (second agent))

(define (dat-agent agent)
  (third agent))
;; --------------------------------------------

;; --------------------------------------------
;; These functions are the selectors
;; from agent families 
;; --------------------------------------------
(define (fitness<-agents agfamily)
  (map fitness-agent  agfamily))
;; This functions makes a list of the
;; members control
(define (ctrls<-agents agfamily)
  (map ctrl-agent agfamily))
;; This functions makes a list of the
;; members control
(define (dat<-agents agfamily)
  (map dat-agent agfamily))
;; --------------------------------------------

;; ------------------------------------------
;; This function evaluates the numeric simulation
;; of the controller under fitness function
;; and returns an agent
;; ------------------------------------------
(define (fitplot-controller ctrl-law environment [erase? #t])
  (let* ( [simdat  (simulate-environment ctrl-law  environment erase?)]
          [fitness (fitness simdat environment)])
    (plot-simdata simdat)
    fitness
    ))

;; ------------------------------------------
;; This function evaluates the numeric simulation
;; of the controller under fitness function
;; and returns an agent
;; ------------------------------------------
(define (evaluate-controller ctrl-law environment)
  (let* ( [simdat  (simulate-environment ctrl-law  environment)]
          [fitness (fitness simdat environment)])
     (make-agent
     fitness
     ctrl-law
     simdat)
    ))
;; ------------------------------------------
;; This function evaluates a family of controller
;; and returns a sorted list agents
;; ------------------------------------------
(define (evaluate-controllers family environment)
  (let ([evaluated-family
         (map (lambda (ctrl)
                (evaluate-controller ctrl environment)) family)])
    (sort-agents evaluated-family)))
;; ------------------------------------------


;; ------------------------------------------
;; This function sort the agents in ascending
;; mode in function of the fitness
;; ------------------------------------------
(define (sort-agents agents)
  (sort agents
        (lambda (agent-0 agent-1)
          (< (fitness-agent  agent-0)
             (fitness-agent  agent-1)
             ))))

;; ------------------------------------------
;; This function compute the arithmetic media
;; ------------------------------------------
(define (arithmetic-media data)
  (let* ([n (length data)]
         [sumatorie (foldl + 0.0 data)]
         )
    (/ sumatorie n)))

(define (media-agents agents)
  (let ([fitness-list (fitness<-agents agents)])
    (arithmetic-media fitness-list)))


; --------------------------------------------
;;  Evaluation function: This function evaluate
;; a family of controllers and make the reports
;; --------------------------------------------
(define (evaluate family environment)
  (evaluate-controllers family environment))

;; (define (evaluate family environment)
;;    (let* ([agents (evaluate-family family environment)]
;;           [fitctrl-list (make-fitctrl-list agents)]
;;           [datmem (dat-agentfam agents)])
;;     ;(make-datalog (name-envrsys environment) simplemem)
;;     ;(make-simdatlog (name-envrsys environment) datmem saved-members-per-family)
;;      ; simplemem
;;      ;fitctrl-list
;;      ;datmem
;;     enviroment
;;      ))


;; --------------------------------------------
;; Debugg Code Low Level
;; --------------------------------------------
;; (require "../plant/plant-library.rkt"
;;          "../environment/environment-library.rkt"
;;          )
;; (define _plant (FirstOrder))
;; (define _ctrl-1 '(* 10 e))
;; (define _ctrl-0 '(* 5 e))
;; (define _ref 10)

;; (define _prms (make-env-prmtrs)) 
;; (define _t0   0.0)
;; (define _tf   2.0)
;; (define _dt   100e-3)
;; (_prms 'set-dt! _dt)
;; (_prms 'set-t0! _t0)
;; (_prms 'set-tf! _tf)

;; (define _enviro (make-environment  _plant _ref _prms))

;; ;; ;; Evaluates a simple control law
;; (fitness-agent (evaluate-controller _ctrl-0 _enviro))
;; --------------------------------------------
;; ;; ;; Controller definitions
;;  (define _ctrl-2 '(* 5.99 e))
;;  (define _ctrl-3 '(* -2 e))
;;  (define _ctrl-4 '(* 10.1 e))
;; ;; ;; Build the controllers family
;;  (define _family (list _ctrl-0
;;                        _ctrl-1
;;                        _ctrl-2
;;                        _ctrl-3
;;                        _ctrl-4))

;; ;; ;; Evaluate the family; Returns the sorted evaluated family
;;  (define _evfamily (evaluate-controllers _family _enviro))
;;  (fitness-agents _evfamily)
;;  (ctrl-agents _evfamily)                      

;;  (define _familyext (append _family _family))
;;  (define _evfamilyext (evaluate-controllers _familyext _enviro))
;;  (fitness-agents _evfamilyext)
;;  (ctrl-agents _evfamilyext)  
;; --------------------------------------------
;; (require "../ctrl/ctrl-library.rkt")
;; (require "../plant/plant-library.rkt"
;;          "../environment/environment-library.rkt"
;;          )
;; (define _plant (FirstOrder))
;; (define _ctrl-1 '(* 10 e))
;; (define _ctrl-0 '(* 5 e))
;; (define _ref 10)

;; (define _prms (make-env-prmtrs)) 
;; (define _t0   0.0)
;; (define _tf   0.03)
;; (define _dt   10e-6)
;; (_prms 'set-dt! _dt)
;; (_prms 'set-t0! _t0)
;; (_prms 'set-tf! _tf)
;; (define _enviro (make-environment  _plant _ref _prms))

;; (define c3 '(/
;;   (* -8.17959273265565 (tanh (- (- (- (- (/ -6.00937164154586 e)))))))
;;   (/
;;    (+ (- (- (/ 7.964583471564893 e))) (+ (- (- e)) (- (- e))))
;;    (/ (- (/ (- e) (- e))) (- (- (* 8.466044003362107 e)))))))
;; (define c4 '(*
;;   (*
;;    -7.139899480412502
;;    (tanh
;;     (*
;;      (- (/ (+ (+ 1.77635940943909 e) (- e)) (- (- e))))
;;      (*
;;       (+
;;        (- (+ -2.8234904974899298 e))
;;        (/ (* 8.666340481163662 e) (+ -7.210549563121589 e)))
;;       (*
;;        (+ (- e) (- e))
;;        (/ (* 1.8021399376087626 e) (+ 3.3980900391011346 e)))))))
;;   (-
;;    (*
;;     (* (+ (- e) (- e)) (- (/ 7.970123728221687 e)))
;;     (+
;;      (* (- e) (* -8.489768082711791 e))
;;      (/ (/ 3.68128126620001 e) (/ 7.526319181889853 e)))))))
;; (define c5 '(/
;;   (*
;;    -2.6010691144090083
;;    (tanh
;;     (*
;;      (+
;;       (/
;;        (* (/ -0.7826975692997422 e) (* -3.06098083888274 e))
;;        (/ (- e) (* -1.5758295608154835 e)))
;;       (- (/ (+ 2.61982840600524 e) (/ 9.457132831933823 e))))
;;      (/
;;       (* (/ (* 0.5877502780063217 e) (+ 4.390210279534513 e)) (- (- e)))
;;       (/
;;        (+ (/ -3.535733440758789 e) (+ -5.011432977015631 e))
;;        (- (/ 7.6392980080503 e)))))))
;;   (/
;;    (- (/ (+ (- e) (+ 1.1876413056229715 e)) (- (+ 5.791558964327972 e))))
;;    (*
;;     (- (+ (* -8.428451636135097 e) (+ 0.05868848697450346 e)))
;;     (+
;;      (- (* 6.278083982374863 e))
;;      (+ (+ 4.664344943636971 e) (/ -8.402130503124358 e)))))))
;; (define _ctrlfixfamily (list c3 c4 c5)) ;
;; (define _evfixfamily (evaluate-controllers _ctrlfixfamily _enviro))
;; ;; '(0.04174182119514204 3.158328073838223 10.000267035415577) @ t[0,0.1] dt= 0.1e-
;; (plot-environment c3 _enviro)
;; (plot-environment c4 _enviro )
;; (plot-environment c5 _enviro )
;; (fitness-agents _evfixfamily)

;; --------------------------------------------
;; (require "../ctrl/ctrl-library.rkt")
;; (require "../plant/plant-library.rkt"
;;          "../environment/environment-library.rkt"
;;          )
;; (define _plant (FirstOrder))
;; (define _ctrl-1 '(* 10 e))
;; (define _ctrl-0 '(* 5 e))
;; (define _ref 10)

;; (define _prms (make-env-prmtrs)) 
;; (define _t0   0.0)
;; (define _tf   0.05)
;; (define _dt   2000e-6)
;; (_prms 'set-dt! _dt)
;; (_prms 'set-t0! _t0)
;; (_prms 'set-tf! _tf)


;; ;; Evaluate one controller only
;; (define _enviro (make-environment  _plant _ref _prms))
;; (define _data_sim (simulate-environment _ctrl-1 _enviro #f))




;; ;Evaluate a family of controllers 
;; ;; (define _ctrlfamily (make-controller-family 10)) 
;; ;; (define _evfamily (evaluate-controllers  _ctrlfamily _enviro))
;; ;; (fitness<-agents _evfamily)
;; ;; (define _bestc (ctrl-agent (first _evfamily)))
;; ;; (define _worstc (ctrl-agent (first (reverse _evfamily))))
;; ;; (plot-environment _bestc _enviro)
;; ;; (plot-environment _worstc _enviro)
;; ;;--------------------------------------------
