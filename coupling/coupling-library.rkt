#lang racket
(require "../evaluator/evaluator-library.rkt")
(require "../report/report-library.rkt")
(require "../environment/environment-library.rkt")
(require "../mutation/mutation-library.rkt")

(provide evaluate
         evolve
         evolution
         fitness-individual
         controller-individual
         evaluate-controller
         fitplot-controller
         fitness-agent
         ctrl-agent 
         dat-agent
         set!-norm2f
         set!-itaef
         set!-dspeedf
         make-data-5best
        ) 

(define members-saved-per-family 3)

(define (make-individual fitness indv)
  (list fitness indv))
(define (fitness-individual indv)
  (first indv))
(define (controller-individual indv)
  (second indv))


(define (fitness-population population)
  (map first population))

(define (family-population population)
  (map second population))


;; Requires agents (fitness ctrl and data-simulation)
(define (make-agents-reports agents plant-name)
  (let*([fitness-list (fitness<-agents agents)]
        [ctrls-list (ctrls<-agents agents)]
        [individuals (map make-individual
                          fitness-list
                          ctrls-list)]
         ;[datmem   (dat<-agents agents)]
         )
    (make-datalog plant-name individuals)
    ;(make-simdatlog plant-name datmem members-saved-per-family)
    individuals))

;; Returns an individuals (fitness and controll)
(define (evaluate family environment)
  (let* ([plant-name (environment 'name)]
         [agents (evaluate-controllers family environment)]
         [population (make-agents-reports agents plant-name)])
    population))


;; Return a family of controllers
(define (evolve population)
   (genetic-operations population))
  

(define (evolution family environment times)
  (let*([population (evaluate family environment)])
    ;; (plot-environment (first (first population)) environment #f)
    (if (zero? times)
        (let([list-pop (list population) ])
          (write-last-family (environment 'name) population)
          list-pop)
        (let([newfam (evolve population)])
          (list*
           population
           (evolution newfam environment (sub1 times))
                
           )
          ))))

(define (make-sim env indv gen idx)
  (let* ([ctrl (second indv)]
         [new-name (string-append
                    "g" (number->string gen)
                    "b" (number->string idx))])
  (env 'set-name! new-name)
  (simulate-environment  ctrl  env #f)
  ))

(define (make-data environment population gen)
  (map (lambda (ctrl idx)
         (make-sim environment ctrl gen idx))
       (take population 5)
       (range 5)
       ))
       
(define (make-data-5best hpopulation environment)
  (let* ([size (length hpopulation)]
         [lrange  (range size)]
         )
    (map (lambda (population gen)
           (make-data environment population gen))
         hpopulation lrange)))





;; (require "../plant/plant-library.rkt"
;;         "../ctrl/ctrl-library.rkt")
;; (define evolution-times 10)

;; ;; -----------------
;; ;; TESTING ENVIRONMENT OK
;; ;; ----------------
;; (define _plant (FirstOrder))
;; (define controller (make-ctrl))
;; (define _ref 2)

;; (define _prms (make-env-prmtrs)) 
;; (define _t0   0.0)
;; (define _tf   5.0)
;; (define _dt   10e-3)
;; (_prms 'set-dt! _dt)
;; (_prms 'set-t0! _t0)
;; (_prms 'set-tf! _tf)
;; (define _enviro (make-environment  _plant _ref _prms)) 

;; (define family-ctrl (make-controller-family 20))

;; (define lastpopulation (evolution family-ctrl _enviro evolution-times))
;; (define firstpopulation (evaluate family-ctrl _enviro))

;; (define best-firstctrl (controller-individual (first firstpopulation)))
;; (define best-lastctrl (controller-individual (first lastpopulation)))

;; (plot-environment   best-firstctrl _enviro)
;; (plot-environment   best-lastctrl _enviro)
  

;; (_prms 'set-tf! 0.5)
;; (plot-environment   best-firstctrl _enviro)
;; (plot-environment   best-lastctrl _enviro)
